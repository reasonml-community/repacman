open Reprocessing;

type direction =
  | Up
  | Down
  | Left
  | Right;

type state = {
  direction,
  nextDirection: direction,
  pos: (float, float)
};

let setup = env => {
  Env.size(~width=200, ~height=200, env);
  {direction: Down, nextDirection: Down, pos: (25., 25.)};
};

let grid = [0, 1, 2, 3, 4, 5, 6, 7];

let keyDirection = (state, env) => {
  let isUp = Env.keyPressed(Events.Up, env);
  let isDown = Env.keyPressed(Events.Down, env);
  let isRight = Env.keyPressed(Events.Right, env);
  let isLeft = Env.keyPressed(Events.Left, env);
  switch (isUp, isDown, isRight, isLeft) {
  | (true, false, false, false) => Up
  | (false, true, false, false) => Down
  | (false, false, true, false) => Right
  | (false, false, false, true) => Left
  | _ => state.nextDirection
  };
};

let getDirection = ({direction, pos: (x, y), nextDirection}) => {
  let intersection = (mod_float(x, 25.), mod_float(y, 25.));
  switch (intersection) {
  | (0., 0.) => nextDirection
  | _ => direction
  };
};

let move = ({direction, pos: (x, y), nextDirection}) => {
  let x =
    switch (direction, x) {
    | (Right, 200.) => 0.
    | (Left, 0.) => 200.
    | _ => x
    };
  let y =
    switch (direction, y) {
    | (Up, 0.) => 200.
    | (Down, 200.) => 0.
    | _ => y
    };
  let direction = getDirection({direction, nextDirection, pos: (x, y)});
  
  switch (direction) {
  | Up => {direction, nextDirection, pos: (x, y -. 1.)}
  | Down => {direction, nextDirection, pos: (x, y +. 1.)}
  | Right => {direction, nextDirection, pos: (x +. 1., y)}
  | Left => {direction, nextDirection, pos: (x -. 1., y)}
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
    grid
  );
  noStroke(env);
  fill(Constants.red, env);
  let nextDirection = keyDirection(state, env);
  let state = move({...state, nextDirection});
  rectf(~pos=state.pos, ~width=10., ~height=10., env);
  state;
};

Reprocessing.run(~screen="canvas", ~setup, ~draw, ());
