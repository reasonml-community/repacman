open Reprocessing;

type direction =
  | Up
  | Down
  | Left
  | Right;

type state = {
  direction,
  pos: (float, float),
};

let setup = env => {
  Env.size(~width=200, ~height=200, env);
  {direction: Down, pos: (25., 25.)};
};

let grid = [0, 1, 2, 3, 4, 5, 6, 7];

let keyDirection = (state, env) => {
  let isUp = Env.key(Events.Up, env);
  let isDown = Env.key(Events.Down, env);
  switch (isUp, isDown) {
  | (true, false) => Up
  | (false, true) => Down
  | _ => state.direction
  };
};

let move = ({direction, pos: (x, y)}) => {
  let x = x == 200. ? 0. : x;
  let y = y == 200. ? 0. : y;
  switch (direction) {
  | Up => {direction, pos: (x, y -. 1.)}
  | Down => {direction, pos: (x, y +. 1.)}
  | Right => {direction, pos: (x +. 1., y)}
  | Left => {direction, pos: (x -. 1., y)}
  };
};

let draw = (state, env) => {
  open Draw;
  background(Constants.white, env);
  stroke(Constants.black, env);
  List.iter(
    gridPoint => {
      line(~p1=(gridPoint * 25, 0), ~p2=(gridPoint * 25, 200), env);
      line(~p1=(0, gridPoint * 25), ~p2=(200, gridPoint * 25), env);
    },
    grid,
  );
  noStroke(env);
  fill(Constants.red, env);
  let state = {...state, direction: keyDirection(state, env)};
  let state = move(state);
  rectf(~pos=state.pos, ~width=10., ~height=10., env);
  state;
};

Reprocessing.run(~screen="canvas", ~setup, ~draw, ());