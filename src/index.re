open Reprocessing;

type direction =
  | Up
  | Down
  | Left
  | Right;

type pos = (float, float);

type agent = {
  direction,
  nextDirection: direction,
  pos,
};

type fruit = {
  pos,
  points: int,
};

type state = {
  pacman: agent,
  score: int,
  fruits: list(fruit),
  paused: bool,
};

let initialState = {
  pacman: {
    direction: Down,
    nextDirection: Down,
    pos: (25., 25.),
  },
  score: 0,
  fruits: [],
  paused: false,
};

let gridSize = 200.;

let count = 8;

let gridStep = gridSize /. float_of_int(count);

let grid = [0, 1, 2, 3, 4, 5, 6, 7];

let keyDirection = (state: agent, env) => {
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
  let intersection = (
    x == 0. || x == 200. ? 1. : mod_float(x, 25.),
    y == 0. || y == 200. ? 1. : mod_float(y, 25.),
  );
  let reverseDirection =
    switch (direction, nextDirection) {
    | (Up, Down) => true
    | (Down, Up) => true
    | (Left, Right) => true
    | (Right, Left) => true
    | _ => false
    };
  if (reverseDirection) {
    nextDirection;
  } else {
    switch (intersection) {
    | (0., 0.) => nextDirection
    | _ => direction
    };
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
    | (Down, 198.) => 0.
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

let drawPacman = (state: agent, env) => {
  open Draw;
  fill(Constants.red, env);
  let nextDirection = keyDirection(state, env);
  let state = move({...state, nextDirection});
  let (x, y) = state.pos;
  rectf(~pos=(x -. 5., y -. 5.), ~width=10., ~height=10., env);
  state;
};

let drawPacmanNoMove = (state: agent, env) => {
  open Draw;
  fill(Constants.red, env);
  let (x, y) = state.pos;
  rectf(~pos=(x -. 5., y -. 5.), ~width=10., ~height=10., env);
  ();
};

let createRandomFruit = () => {
  let x = float_of_int(Utils.random(~min=1, ~max=count - 1)) *. gridStep;
  let y = float_of_int(Utils.random(~min=1, ~max=count - 1)) *. gridStep;
  {pos: (x, y), points: 1};
};

let getRandomFruit = () =>
  switch (Utils.random(~min=1, ~max=100)) {
  | 1 => Some(createRandomFruit())
  | _ => None
  };

let getUpdatedListOfFruitsWithChance = fruits =>
  switch (fruits) {
  | [_, _, _] => fruits
  | _ =>
    let randomFruit = getRandomFruit();
    switch (randomFruit) {
    | Some(f) => [f, ...fruits]
    | None => fruits
    };
  };

let drawFruit = (fruit: fruit, env) =>
  Draw.(ellipsef(~center=fruit.pos, ~radx=5., ~rady=5., env));

let isColliding = (fruitPos, pacmanPos) => fruitPos == pacmanPos;

let scoreDisplay = (~score: int, env) =>
  Draw.(text(~body=string_of_int(score), ~pos=(3, 3), env));

let filterFruitsAndGetPoints = state =>
  state.fruits
  |> List.fold_left(
       ((fruits, score), fruit) =>
         if (isColliding(fruit.pos, state.pacman.pos)) {
           (fruits, fruit.points + score);
         } else {
           ([fruit, ...fruits], score);
         },
       ([], state.score)
     );

let drawGrid = env =>
  Draw.(
    grid
    |> List.iter(gridPoint => {
         line(~p1=(gridPoint * 25, 0), ~p2=(gridPoint * 25, 200), env);
         line(~p1=(0, gridPoint * 25), ~p2=(200, gridPoint * 25), env);
       })
  );

let setup = env => {
  Env.size(~width=200, ~height=200, env);
  initialState;
};

let draw = (state, env) => {
  open Draw;
  background(Constants.white, env);
  stroke(Constants.black, env);
  drawGrid(env);
  noStroke(env);
  if (Env.keyPressed(Events.Escape, env)) {
    initialState;
  } else {
    let state =
      switch (state.paused, Env.keyPressed(Events.Space, env)) {
      | (true, true) => {...state, paused: false}
      | (false, true) => {...state, paused: true}
      | (true, false) => {...state, paused: true}
      | (false, false) => {...state, paused: false}
      };
    if (state.paused) {
      scoreDisplay(~score=state.score, env);
      state.fruits |> List.iter(fruit => drawFruit(fruit, env));
      drawPacmanNoMove(state.pacman, env);
      text(~body="Paused", ~pos=(45, 90), env);
      state;
    } else {
      let (fruits, newScore) = filterFruitsAndGetPoints(state);
      scoreDisplay(~score=newScore, env);
      let fruits = getUpdatedListOfFruitsWithChance(fruits);
      fruits |> List.iter(fruit => drawFruit(fruit, env));
      let pacmanState = drawPacman(state.pacman, env);
      {...state, fruits, score: newScore, pacman: pacmanState};
    };
  };
};

run(~setup, ~draw, ());