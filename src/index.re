open Reprocessing;

let rec rangef = (start: float, end_: float) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...rangef(start +. 1., end_)];
  };

module GridSizes = {
  let gridWidth = 400;
  let gridWidthf = float_of_int(gridWidth);
  let gridHeight = 400;
  let gridHeightf = float_of_int(gridHeight);
  let numOfColumns = 20;
  let numOfColumnsf = float_of_int(numOfColumns);
  let stepSizef = gridWidthf /. numOfColumnsf;
  let gridColumnsf = rangef(0., numOfColumnsf);
};

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
    pos: (GridSizes.gridWidthf /. 2., GridSizes.gridHeightf /. 2.),
  },
  score: 0,
  fruits: [],
  paused: false,
};

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
    x == 0. || x == GridSizes.gridWidthf ?
      1. : mod_float(x, GridSizes.stepSizef),
    y == 0. || y == GridSizes.gridHeightf ?
      1. : mod_float(y, GridSizes.stepSizef),
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
    switch (direction) {
    | Right => x == GridSizes.gridWidthf ? 0. : x
    | Left => x == 0. ? GridSizes.gridWidthf : x
    | _ => x
    };
  let y =
    switch (direction) {
    | Up => y == 0. ? GridSizes.gridHeightf : y
    | Down => y == GridSizes.gridHeightf ? 0. : y
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
  ellipsef(~center=state.pos, ~radx=15., ~rady=15., env);
  state;
};

let drawPacmanNoMove = (state: agent, env) => {
  open Draw;
  fill(Constants.red, env);
  ellipsef(~center=state.pos, ~radx=15., ~rady=15., env);
  ();
};

let createRandomFruit = () => {
  let x =
    Utils.(
      round(randomf(~min=1., ~max=GridSizes.numOfColumnsf -. 1.))
      *. GridSizes.stepSizef
    );
  let y =
    Utils.(
      round(randomf(~min=1., ~max=GridSizes.numOfColumnsf -. 1.))
      *. GridSizes.stepSizef
    );
  {pos: (x, y), points: 1};
};

let rec getRandomFruit = validatePosition =>
  switch (Utils.random(~min=1, ~max=100)) {
  | 1 =>
    let newFruit = createRandomFruit();
    let isPositionTaken = validatePosition(newFruit);
    if (isPositionTaken) {
      getRandomFruit(validatePosition);
    } else {
      Some(newFruit);
    };
  | _ => None
  };

let getUpdatedListOfFruitsWithChance = fruits =>
  switch (fruits) {
  | [_, _, _] => fruits
  | _ =>
    let randomFruit =
      getRandomFruit(newFruit =>
        fruits |> List.exists(fruit => newFruit.pos == fruit.pos)
      );
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
    GridSizes.gridColumnsf
    |> List.iter(gridPoint => {
         let xStart = (gridPoint *. GridSizes.stepSizef, 0.);
         let xEnd = (gridPoint *. GridSizes.stepSizef, GridSizes.gridHeightf);
         linef(~p1=xStart, ~p2=xEnd, env);
         let yStart = (0., gridPoint *. GridSizes.stepSizef);
         let yEnd = (GridSizes.gridWidthf, gridPoint *. GridSizes.stepSizef);
         linef(~p1=yStart, ~p2=yEnd, env);
       })
  );

let setup = env => {
  Env.size(~width=GridSizes.gridWidth, ~height=GridSizes.gridHeight, env);
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