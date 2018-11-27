//! Displays a shaded sphere to the user.

use amethyst;
#[macro_use]
extern crate serde;

use amethyst::{
    animation::{
        get_animation_set, AnimationBundle, AnimationCommand, AnimationSet, AnimationSetPrefab,
        DeferStartRelation, EndControl, StepDirection,
    },
    assets::{PrefabLoader, PrefabLoaderSystem, RonFormat},
    core::{Transform, TransformBundle},
    ecs::prelude::Entity,
    input::{get_key, is_close_requested, is_key_down},
    prelude::*,
    renderer::{DrawShaded, ElementState, PosNormTex, VirtualKeyCode},
    utils::{application_root_dir, scene::BasicScenePrefab},
    State,
};

type MyPrefabData = (
    Option<BasicScenePrefab<Vec<PosNormTex>>>,
    Option<AnimationSetPrefab<AnimationId, Transform>>,
);

#[derive(Eq, PartialOrd, PartialEq, Hash, Debug, Copy, Clone, Deserialize, Serialize)]
enum AnimationId {
    Scale,
    Rotate,
    Translate,
}

#[derive(State, Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Custom {
    name: String,
}

#[derive(State, Clone, Debug)]
enum State {
    Example,
    Number(u32),
    Custom(Custom),
}

struct Example {
    pub sphere: Option<Entity>,
    rate: f32,
    current_animation: AnimationId,
}

impl Default for Example {
    fn default() -> Self {
        Example {
            sphere: None,
            rate: 1.0,
            current_animation: AnimationId::Translate,
        }
    }
}

impl<S> StateHandler<S, StateEvent> for Example {
    fn on_start(&mut self, world: &mut World) {
        // Initialise the scene with an object, a light and a camera.
        let prefab_handle = world.exec(|loader: PrefabLoader<'_, MyPrefabData>| {
            loader.load("prefab/animation.ron", RonFormat, (), ())
        });
        self.sphere = Some(world.create_entity().with(prefab_handle).build());
    }

    fn handle_event(&mut self, world: &mut World, event: &StateEvent) -> Trans<S> {
        if let StateEvent::Window(event) = &event {
            if is_close_requested(&event) || is_key_down(&event, VirtualKeyCode::Escape) {
                return Trans::Quit;
            }
            match get_key(&event) {
                Some((VirtualKeyCode::Space, ElementState::Pressed)) => {
                    add_animation(
                        world,
                        self.sphere.unwrap(),
                        self.current_animation,
                        self.rate,
                        None,
                        true,
                    );
                }

                Some((VirtualKeyCode::D, ElementState::Pressed)) => {
                    add_animation(
                        world,
                        self.sphere.unwrap(),
                        AnimationId::Translate,
                        self.rate,
                        None,
                        false,
                    );
                    add_animation(
                        world,
                        self.sphere.unwrap(),
                        AnimationId::Rotate,
                        self.rate,
                        Some((AnimationId::Translate, DeferStartRelation::End)),
                        false,
                    );
                    add_animation(
                        world,
                        self.sphere.unwrap(),
                        AnimationId::Scale,
                        self.rate,
                        Some((AnimationId::Rotate, DeferStartRelation::Start(0.666))),
                        false,
                    );
                }

                Some((VirtualKeyCode::Left, ElementState::Pressed)) => {
                    get_animation_set::<AnimationId, Transform>(
                        &mut world.write_storage(),
                        self.sphere.unwrap().clone(),
                    )
                    .unwrap()
                    .step(self.current_animation, StepDirection::Backward);
                }

                Some((VirtualKeyCode::Right, ElementState::Pressed)) => {
                    get_animation_set::<AnimationId, Transform>(
                        &mut world.write_storage(),
                        self.sphere.unwrap().clone(),
                    )
                    .unwrap()
                    .step(self.current_animation, StepDirection::Forward);
                }

                Some((VirtualKeyCode::F, ElementState::Pressed)) => {
                    self.rate = 1.0;
                    get_animation_set::<AnimationId, Transform>(
                        &mut world.write_storage(),
                        self.sphere.unwrap().clone(),
                    )
                    .unwrap()
                    .set_rate(self.current_animation, self.rate);
                }

                Some((VirtualKeyCode::V, ElementState::Pressed)) => {
                    self.rate = 0.0;
                    get_animation_set::<AnimationId, Transform>(
                        &mut world.write_storage(),
                        self.sphere.unwrap().clone(),
                    )
                    .unwrap()
                    .set_rate(self.current_animation, self.rate);
                }

                Some((VirtualKeyCode::H, ElementState::Pressed)) => {
                    self.rate = 0.5;
                    get_animation_set::<AnimationId, Transform>(
                        &mut world.write_storage(),
                        self.sphere.unwrap().clone(),
                    )
                    .unwrap()
                    .set_rate(self.current_animation, self.rate);
                }

                Some((VirtualKeyCode::R, ElementState::Pressed)) => {
                    self.current_animation = AnimationId::Rotate;
                }

                Some((VirtualKeyCode::S, ElementState::Pressed)) => {
                    self.current_animation = AnimationId::Scale;
                }

                Some((VirtualKeyCode::T, ElementState::Pressed)) => {
                    self.current_animation = AnimationId::Translate;
                }

                _ => {}
            };
        }
        Trans::None
    }
}

fn main() -> amethyst::Result<()> {
    amethyst::start_logger(Default::default());

    let app_root = application_root_dir();

    let display_config_path = format!(
        "{}/examples/animation/resources/display_config.ron",
        app_root
    );

    let resources = format!("{}/examples/assets/", app_root);

    let game_data = GameDataBuilder::default()
        .with(PrefabLoaderSystem::<MyPrefabData>::default(), "", &[])
        .with_bundle(AnimationBundle::<AnimationId, Transform>::new(
            "animation_control_system",
            "sampler_interpolation_system",
        ))?
        .with_bundle(TransformBundle::new().with_dep(&["sampler_interpolation_system"]))?
        .with_basic_renderer(display_config_path, DrawShaded::<PosNormTex>::new(), false)?;

    let mut game = Application::build(resources)?
        .with_state(State::Example, Example::default())?
        .build(game_data)?;

    game.run();
    Ok(())
}

fn add_animation(
    world: &mut World,
    entity: Entity,
    id: AnimationId,
    rate: f32,
    defer: Option<(AnimationId, DeferStartRelation)>,
    toggle_if_exists: bool,
) {
    let animation = world
        .read_storage::<AnimationSet<AnimationId, Transform>>()
        .get(entity)
        .and_then(|s| s.get(&id))
        .cloned()
        .unwrap();
    let mut sets = world.write_storage();
    let control_set = get_animation_set::<AnimationId, Transform>(&mut sets, entity).unwrap();
    match defer {
        None => {
            if toggle_if_exists && control_set.has_animation(id) {
                control_set.toggle(id);
            } else {
                control_set.add_animation(
                    id,
                    &animation,
                    EndControl::Normal,
                    rate,
                    AnimationCommand::Start,
                );
            }
        }

        Some((defer_id, defer_relation)) => {
            control_set.add_deferred_animation(
                id,
                &animation,
                EndControl::Normal,
                rate,
                AnimationCommand::Start,
                defer_id,
                defer_relation,
            );
        }
    }
}
