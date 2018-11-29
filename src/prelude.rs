//! Contains common types that can be glob-imported (`*`) for convenience.

pub use crate::{
    callback_queue::{Callback, CallbackQueue},
    config::Config,
    core::{SystemExt, WithNamed},
    dynamic,
    ecs::prelude::{Builder, World},
    state_event::StateEvent,
    game_data::{DataInit, GameData, GameDataBuilder},
};

#[cfg(not(feature = "dynamic_app"))]
pub use crate::{
    app::{Application, ApplicationBuilder, CoreApplication},
    state::{
        EmptyState, EmptyTrans, SimpleState, SimpleTrans, State, StateData, StateMachine, Trans,
        TransEvent,
    },
};

#[cfg(feature = "dynamic_app")]
pub use self::dynamic::*;
