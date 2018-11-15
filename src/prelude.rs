//! Contains common types that can be glob-imported (`*`) for convenience.

pub use {
    app::{Application, ApplicationBuilder, CoreApplication},
    callback_queue::{Callback, CallbackQueue},
    config::Config,
    core::WithNamed,
    ecs::prelude::{Builder, World},
    game_data::{DataInit, GameData, GameDataBuilder},
    state::{
        EmptyState, EmptyTrans, SimpleState, SimpleTrans, State, StateData, Trans, TransEvent,
    },
    state_event::StateEvent,
    system_extra::SystemExtra,
};
