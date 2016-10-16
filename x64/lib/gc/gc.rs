#[macro_use(lazy_static, __lazy_static_create)]
extern crate lazy_static;

use std::sync::Mutex;
use std::mem::transmute;
use std::mem::size_of;
use std::ops::DerefMut;


#[repr(C)]
struct Env {
    outer_env : Option<Box<Env>>, 
    arg_code : i64, 
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
    arg_code : i64, 
    arg_env : usize,
    mark : bool,
    free : bool
}

// guarantee that the two structs have the same size
// TODO
//assert_eq!(size_of::<NurseryEnv>(), size_of::<NurseryEnvInit>());

//default empty NurseryEnvInit
const NURSERY_ENV_INIT : NurseryEnvInit = NurseryEnvInit { 
    outer_env: 0usize, 
    arg_code: 0, 
    arg_env: 0usize, 
    mark: false,
    free: true
};

//number of blocks in the nursery
const NURSERY_SIZE : usize = 10000;//TODO: choose size

struct Nursery {
    nursery_array : [NurseryEnv; NURSERY_SIZE], 
    nursery_index : usize
}

impl Nursery {
    
    // gets the next free environment in the nursery if there is one
    fn get_next(&mut self) -> Option<&Env> {
        let mut result = NURSERY_SIZE;
        let mut idx = self.nursery_index;
        
        while idx < NURSERY_SIZE {
            let ref env = self.nursery_array[idx];
            if env.free { 
                result = idx;
            }
        }
        
        self.nursery_index = idx;
        
        if  idx < NURSERY_SIZE {
            let ref mut res = self.nursery_array[idx];
            res.free = false;
            Some(&res.env)
        } else {
            None   
        }
    }
}

// we will grow the old_gen whenever it gets full
//TODO: investigate overhead
lazy_static! {
    static ref NURSERY : Mutex<Nursery> = Mutex::new(Nursery{
        // this is the 
        nursery_array : unsafe { transmute([NURSERY_ENV_INIT; NURSERY_SIZE]) },
        nursery_index : 0
    });
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


//gets the next free environment if there is one
fn get_next() -> &'static Env {
    let mut guard = NURSERY.lock().unwrap();
    let mut nursery = guard.deref_mut();
    match nursery.get_next() {
        Some(e) => e,
        None => get_next()
    }
}