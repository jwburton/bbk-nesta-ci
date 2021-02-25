import Empirica from "meteor/empirica:core";
import "./bots.js";
import "./callbacks.js";
import { questions } from "./constants";

// gameInit is where the structure of a game is defined.
// Just before every game starts, once all the players needed are ready, this
// function is called with the treatment and the list of players.
// You must then add rounds and stages to the game, depending on the treatment
// and the players. You can also get/set initial values on your game, players,
// rounds and stages (with get/set methods), that will be able to use later in
// the game.
Empirica.gameInit(game => {

  // randomize order of questions/events being predicted
  questions = _.shuffle(questions);

  // create a 5-stage round for each question
  questions.forEach((q) => {
    const round = game.addRound({
      data: {
        question: q,
      }
    });
    round.addStage({
      name: "initial",
      displayName: "Initial Prediction",
      durationInSeconds: 61
    });
    round.addStage({
      name: "social1",
      displayName: "Revision 1",
      durationInSeconds: 61
    });
    round.addStage({
      name: "social2",
      displayName: "Revision 2",
      durationInSeconds: 61
    });
    round.addStage({
      name: "social3",
      displayName: "Revision 3",
      durationInSeconds: 61
    });
    round.addStage({
      name: "final",
      displayName: "Final Prediction",
      durationInSeconds: 61
    });
  });

});