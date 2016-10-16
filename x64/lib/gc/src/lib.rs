#![feature(asm)]
#[macro_use(lazy_static, __lazy_static_create)]
extern crate lazy_static;

use std::sync::Mutex;
use std::mem::transmute;
use std::mem::size_of;
use std::ops::DerefMut;
use std::slice;


#[repr(C)]
struct Env {
    outer_env : Option<Box<Env>>, 
    arg_code : usize, 
    arg_env : Option<Box<Env>>
}

#[repr(C)]
struct NurseryEnv {
    env : Env,
    mark : bool,
    free : bool
}

// Requirement: must be the same size as a NurseryEnv
#[derive(Copy, Clone)]
#[repr(C)]
struct NurseryEnvInit {
    outer_env : usize, 
    arg_code : usize, 
    arg_env : usize,
    mark : bool,
    free : bool
}

//default empty NurseryEnvInit
const NURSERY_ENV_INIT : NurseryEnvInit = NurseryEnvInit { 
    outer_env: 0usize, 
    arg_code: 0usize, 
    arg_env: 0usize, 
    mark: false,
    free: true
};

//number of environments in the nursery
const NURSERY_SIZE : usize = 1000;//TODO: choose size

struct Nursery {
    nursery_array : [NurseryEnv; NURSERY_SIZE], 
    nursery_index : usize
}

impl Nursery {
    
    // gets the next free environment in the nursery if there is one
    fn get_next(&mut self) -> Option<&Env> {
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
            Some(&res.env)
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
    // the numeric address of the start of the nursery
    static ref NURSERY_START : usize = {
        let guard = NURSERY.lock().unwrap();
        let nursery = guard.deref();
        unsafe { transmute(&nursery.nursery_array[0]) }
    };
    // the numeric address at the end of the nursery
    static ref NURSERY_END : usize = 
        match NURSERY_START.checked_add(size_of::<[NurseryEnv; NURSERY_SIZE - 1]>()) {
            Some(x) => x,
            None => panic!("Nursery end address computed to be outside of addressable range.")
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
        mark_and_sweep(nursery);
        if let Some(e) = nursery.get_next() {
            unsafe { transmute(e) }
        } else {
            panic!("Mark and sweep failed to find more free environments!")
        }
    }
}

fn mark_and_sweep(nursery : &mut Nursery) {
    //TODO
    panic!("No more free environments!")
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
            initsp.saturating_sub(transmute(stackptr)).wrapping_div(size_of::<usize>()))
    }
}

// other platforms: TODO
//#[cfg(not(any(target_arch = "x86_64")))]
//fn get_stack() -> &[usize] {...}

