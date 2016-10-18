#![feature(asm)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#[macro_use(lazy_static, __lazy_static_create)]
extern crate lazy_static;

use std::sync::Mutex;
use std::mem::transmute;
use std::mem::size_of;
use std::ops::DerefMut;
use std::slice;


// Contract: outer_env/arg_env must be treated as volitile
#[repr(C)]
struct Env {
    outer_env : Option<Box<Env>>, 
    arg_code : usize, 
    arg_env : Option<Box<Env>>
}

#[repr(C)]
struct NurseryEnv {
    env : Env,
    marked : bool,
    free : bool
}

// Requirement: must be the same size as a NurseryEnv
#[derive(Copy, Clone)]
#[repr(C)]
struct NurseryEnvInit {
    outer_env : usize, 
    arg_code : usize, 
    arg_env : usize,
    marked : bool,
    free : bool
}

//default empty NurseryEnvInit
const NURSERY_ENV_INIT : NurseryEnvInit = NurseryEnvInit { 
    outer_env: 0usize, 
    arg_code: 0usize, 
    arg_env: 0usize, 
    marked: false,
    free: true
};

//number of environments in the nursery
const NURSERY_SIZE : usize = 10;//TODO: choose size

struct Nursery {
    nursery_array : [NurseryEnv; NURSERY_SIZE], 
    nursery_index : usize
}

impl Nursery {
    
    // gets the next free environment in the nursery if there is one
    fn get_next(&mut self) -> Option<&NurseryEnv> {
        
        // index of the first possibly free environemnt
        let start_index = self.nursery_index;
        
        // assume we've gone to the end of the array unless we know otherwise
        self.nursery_index = NURSERY_SIZE;
        
        // for every possibly-free environment, if it's free we'll use its index
        for idx in start_index..NURSERY_SIZE {
            if self.nursery_array[idx].free { 
                self.nursery_index = idx;
                break;
            }
        }
        
        if self.nursery_index < NURSERY_SIZE {
            let ref mut res = self.nursery_array[self.nursery_index];
            res.free = false;
            // this index is no longer free, so move to the next one
            self.nursery_index += 1;
            Some(res)
        } else {
            None   
        }
    }
    
    // perform a mark-and-sweep garbage collection on the nursery
    fn mark_and_sweep(&mut self) {
        
        let stack = get_stack();
        for ptr in stack.iter() {
            if let Some(idx) = self.compute_nursery_index(*ptr) {
                self.mark_env(idx);
            }
        }
        self.sweep();
    }
    
    // recursively mark the environment and its children
    fn mark_env(&mut self, index : usize) {
        let outer_idx_opt : Option<usize>;
        let arg_idx_opt : Option<usize>;
        // we get the environment at the given index, mark it, then check its branches
        // if either one points to a nursery environment, we store its index to check
        {
            let ref mut env = self.nursery_array[index];
            if !env.marked {
                env.marked = true;
                if let Some(box ref e) = env.env.outer_env {
                    outer_idx_opt = Some(unsafe { transmute(e) });
                } else {
                    outer_idx_opt = None;
                }
                if let Some(box ref e) = env.env.arg_env {
                    arg_idx_opt = Some(unsafe { transmute(e) });
                } else {
                    arg_idx_opt = None;
                }
            } else {
                outer_idx_opt = None;
                arg_idx_opt = None;
            }
        }
        // checks must be outside of the lifetime of env
        if let Some(ptr) = outer_idx_opt {
            if let Some(idx) = self.compute_nursery_index(ptr) {
                self.mark_env(idx);
            }
        }
        if let Some(ptr) = arg_idx_opt {
            if let Some(idx) = self.compute_nursery_index(ptr) {
                self.mark_env(idx);
            }
        }
    }
    
    fn sweep(&mut self) {
        for idx in (0..NURSERY_SIZE).rev() {
            println!("checking");
            let ref mut env = self.nursery_array[idx];
            if env.marked {
                env.marked = false;
            } else {
                env.free = true;
                self.nursery_index = idx;
            }
        }
    }
    
    fn get_start(&self) -> usize {
        unsafe { transmute(&self.nursery_array[0]) }
    }
    
    // computes the index of an environemnt from its pointer
    fn compute_nursery_index(&self, ptr : usize) -> Option<usize> {
        // get the memory offset from the first nursery env
        // TODO: make checked sub
        if let Some(offset) = ptr.checked_sub(self.get_start()) {
            // get the index of the environment at that offset
            let idx = offset / size_of::<NurseryEnv>();
            if idx < self.nursery_array.len() 
                        && offset % size_of::<NurseryEnv>() == 0 {
                Some(idx) 
            } else { 
                None 
            }
        } else {
            None
        }
    }
    
}

// we will grow the old_gen whenever it gets full
//TODO: investigate overhead
lazy_static! {
    static ref NURSERY : Mutex<Nursery> = {
        // guarantee that the two structs have the same size
        assert_eq!(size_of::<NurseryEnv>(), size_of::<NurseryEnvInit>());
        Mutex::new(Nursery {
            // this is the most reasonable way to initialize
            nursery_array : unsafe { transmute([NURSERY_ENV_INIT; NURSERY_SIZE]) },
            nursery_index : 0
        })
    };
}

//gets the next free environment and panics if there is none
//Contract: exactly the contents of the cells *e, *(e + 8), and *(e + 16) may be modified.
//              However, none of them may be freed.
#[no_mangle]
pub unsafe extern "C" fn get_next() -> usize {
    get_next_private()
}

fn get_next_private() -> usize {
    let mut guard = NURSERY.lock().unwrap();
    let mut nursery = guard.deref_mut();
    let temp = match nursery.get_next() {
            Some(e) => Some(unsafe { transmute(e) }),
            None => None
    };
    if let Some(envptr) = temp {
        envptr
    } else {
        nursery.mark_and_sweep();
        if let Some(e) = nursery.get_next() {
            unsafe { transmute(e) }
        } else {
            panic!("Mark and sweep failed to find more free environments!")
        }
    }
}


// function that retrieves the stored initial stack pointer
extern {
    fn get_init_sp() -> usize;
}

// Returns a reference to the stack as a slice
#[cfg(any(target_arch = "x86_64"))]
fn get_stack<'a>() -> &'a [usize] {
    let stackptr : *const usize;
    let initsp : usize; 
    unsafe {
        // set stackptr to the stack pointer
        asm!("nop" : "={rsp}"(stackptr));
        // set initsp to the initial stack pointer
        initsp = get_init_sp();
        // calculate the length of the stack and return a slice representing the stack
        slice::from_raw_parts(stackptr, 
            initsp.saturating_sub(transmute(stackptr)) / size_of::<usize>()) //TODO: off-by-1?!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    }
}

// other platforms: TODO
//#[cfg(not(any(target_arch = "x86_64")))]
//fn get_stack() -> &[usize] {...}

