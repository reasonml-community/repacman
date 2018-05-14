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
  pos
};

type fruit = {
  pos,
  points: int
};

type state = {
  pacman: agent,
  score: int,
  fruits: list(fruit)
};

type gridSize = float;

let gridSize = 400.;

let count = 5;

let gridStep = gridSize /. float_of_int(count);

let createRandomFruit = () => {
  let x = float_of_int(Utils.random(~min=1, ~max=count - 1)) *. gridStep;
  let y = float_of_int(Utils.random(~min=1, ~max=count - 1)) *. gridStep;
  {pos: (x, y), points: 1};
};

let setup = env => {
  let fruits = [createRandomFruit(), createRandomFruit(), createRandomFruit()];
  Env.size(~width=int_of_float(gridSize), ~height=int_of_float(gridSize), env);
  {
    pacman: {
      direction: Down,
      nextDirection: Down,
      pos: (gridStep, gridStep)
    },
    score: 0,
    fruits
  };
};

let int_range = (a, b) => {
  let rec int_range_rec = (l, a, b) =>
    if (a > b) {
      l;
    } else {
      int_range_rec([b, ...l], a, b - 1);
    };
  int_range_rec([], a, b);
};

let grid = int_range(0, count);

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
    x == 0. || x == gridSize ? 1. : mod_float(x, gridStep),
    y == 0. || y == gridSize ? 1. : mod_float(y, gridStep)
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
    switch intersection {
    | (0., 0.) => nextDirection
    | _ => direction
    };
  };
};

let move = ({direction, pos: (x, y), nextDirection}) => {
  let x =
    switch (direction, x) {
    | _ when (direction, x) == (Right, gridSize) => 0.
    | _ when (direction, x) == (Left, 0.) => gridSize
    | _ => x
    };
  let y =
    switch (direction, y) {
    | _ when (direction, y) == (Up, 0.) => gridSize
    | _ when (direction, y) == (Down, gridSize -. 2.) => 0.
    | _ => y
    };
  let direction = getDirection({direction, nextDirection, pos: (x, y)});
  switch direction {
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

/* TODO: some random fruit bizniz

   let getRandomFruit = () =>

   let addRandomFruits = fruits => {
     switch fruits {
     | [_, _, _] => fruits
     | _ => //???
     }
   } */
let drawFruit = (fruit: fruit, env) =>
  Draw.(ellipsef(~center=fruit.pos, ~radx=5., ~rady=5., env));

let isColliding = (fruitPos, pacmanPos) => fruitPos == pacmanPos;

let scoreDisplay = (~score: int, env) =>
  Draw.(text(~body=string_of_int(score), ~pos=(3, 3), env));

let draw = (state, env) => {
  open Draw;
  let (fruits, newScore) =
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
  background(Constants.white, env);
  stroke(Constants.black, env);
  grid
  |> List.iter(gridPoint => {
       line(
         ~p1=(gridPoint * int_of_float(gridStep), 0),
         ~p2=(gridPoint * int_of_float(gridStep), int_of_float(gridSize)),
         env
       );
       line(
         ~p1=(0, gridPoint * int_of_float(gridStep)),
         ~p2=(int_of_float(gridSize), gridPoint * int_of_float(gridStep)),
         env
       );
     });
  noStroke(env);
  scoreDisplay(~score=newScore, env);
  List.iter(fruit => drawFruit(fruit, env), state.fruits);
  let pacmanState = drawPacman(state.pacman, env);
  {...state, fruits, score: newScore, pacman: pacmanState};
};

run(~setup, ~draw, ());