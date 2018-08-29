#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;
extern crate rust_tdlib;

use rustler::resource::ResourceArc;
use rustler::schedule::SchedulerFlags;
use rustler::{Encoder, Env, Error, Term};

use rust_tdlib::Tdlib;

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom null;
    }
}

struct ErlTdlib {
    tdlib: Tdlib,
}

unsafe impl Sync for ErlTdlib {}
unsafe impl Send for ErlTdlib {}

fn on_load(env: Env, _info: Term) -> bool {
    resource_struct_init!(ErlTdlib, env);
    true
}

fn new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let tdlib = Tdlib::new();
    let resource = ResourceArc::new(ErlTdlib { tdlib: tdlib });
    Ok((atoms::ok(), resource).encode(env))
}

fn send<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let tdlib: ResourceArc<ErlTdlib> = args[0].decode()?;
    let request: &str = args[1].decode()?;

    tdlib.tdlib.send(request);
    Ok(atoms::ok().encode(env))
}

fn execute<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let tdlib: ResourceArc<ErlTdlib> = args[0].decode()?;
    let request: &str = args[1].decode()?;

    match tdlib.tdlib.execute(request) {
        None => Ok(atoms::null().encode(env)),
        Some(resp) => Ok((atoms::ok(), resp).encode(env)),
    }
}

fn receive<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let tdlib: ResourceArc<ErlTdlib> = args[0].decode()?;
    let timeout: f64 = args[1].decode()?;

    match tdlib.tdlib.receive(timeout) {
        None => Ok(atoms::null().encode(env)),
        Some(resp) => Ok((atoms::ok(), resp).encode(env)),
    }
}

fn set_log_verbosity_level<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let level: i32 = args[0].decode()?;
    match Tdlib::set_log_verbosity_level(level) {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(err) => Ok((atoms::error(), err).encode(env)),
    }
}

fn set_log_max_file_size<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let size: i64 = args[0].decode()?;
    Tdlib::set_log_max_file_size(size);
    Ok(atoms::ok().encode(env))
}

fn set_log_file_path<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path: &str = args[0].decode()?;
    match Tdlib::set_log_file_path(path) {
        true => Ok(atoms::ok().encode(env)),
        false => Ok(atoms::error().encode(env)),
    }
}

rustler_export_nifs!(
    "tdlib_nif",
    [
        ("new", 0, new),
        ("send", 2, send),
        ("execute", 2, execute),
        ("recv", 2, receive, SchedulerFlags::DirtyIo),
        ("set_log_verbosity_level", 1, set_log_verbosity_level),
        ("set_log_max_file_size", 1, set_log_max_file_size),
        ("set_log_file_path", 1, set_log_file_path)
    ],
    Some(on_load)
);
