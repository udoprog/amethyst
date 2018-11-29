//! Contains common types that can be glob-imported (`*`) for convenience.

pub use crate::{
    callback_queue::{Callback, CallbackQueue},
    config::Config,
    core::{SystemExt, WithNamed},
    dynamic,
    ecs::prelude::{Builder, World},
    game_data::{DataInit, GameData, GameDataBuilder},
    state_event::StateEvent,
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
