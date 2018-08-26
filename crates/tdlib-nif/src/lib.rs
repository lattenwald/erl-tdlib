#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;
extern crate rust_tdlib;

use rustler::resource::ResourceArc;
use rustler::{Encoder, Env, Error, Term};
use rustler::schedule::SchedulerFlags;

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

    let resp = tdlib.tdlib.execute(request);
    Ok((atoms::ok(), resp).encode(env))
}

fn receive<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let tdlib: ResourceArc<ErlTdlib> = args[0].decode()?;
    let timeout: f64 = args[1].decode()?;

    match tdlib.tdlib.receive(timeout) {
        None => Ok(atoms::null().encode(env)),
        Some(resp) => Ok((atoms::ok(), resp).encode(env)),
    }
}

rustler_export_nifs!(
    "tdlib_nif",
    [
        ("new", 0, new),
        ("send", 2, send),
        ("execute", 2, execute),
        ("recv", 2, receive, SchedulerFlags::DirtyIo)
    ],
    Some(on_load)
);
