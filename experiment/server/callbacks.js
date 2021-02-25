import Empirica from "meteor/empirica:core";
import initial_networks from './initial_networks';  
//import initial_networks from './6p_initial_nets';  
import scheduled_networks from './scheduled_networks';

// onGameStart is triggered once per game before the game starts, and before
// the first onRoundStart. It receives the game and list of all the players in
// the game.
Empirica.onGameStart(game => {

  game.players.forEach((player, i) => {

    // Assign an id 
    player.set("id",i+1);

    // Assign an avatar 
    player.set("avatar", `/avatars/jdenticon/${player._id}`);
  });  
});

// onRoundStart is triggered before each round starts, and before onStageStart.
// It receives the same options as onGameStart, and the round that is starting.
Empirica.onRoundStart((game, round) => {

  // randomly select one of the initial network structures from the imported initial_networks array
  var random = Math.floor(Math.random() * initial_networks.length);
  var network = initial_networks[random];
  round.set("initial_network", random);

  // For each player... 
  game.players.forEach((player, i) => {

    // Initialise round-level arrays with filler values to store each players' predictions and rationales
    player.round.set("predictions", [50]);
    player.round.set("rationales", [" "]);

    // Assign initial network neighbors based on network json object
    player.round.set("hearsFrom", network.links.filter(s=>s.target===i+1).map(s=>s.source)); 

  });
  
});

// onStageStart is triggered before each stage starts.
// It receives the same options as onRoundStart, and the stage that is starting.
Empirica.onStageStart((game, round, stage) => {
  
  // empty array to hold current data. helps avoid unnecessary db calls and loops.
  let current_data = [];

  // Set stage-level data and put relevant stuff in `current_data`
  game.players.forEach((player, i) => {

    // record id at the stage-level 
    player.stage.set("id", i+1); 

    // record condition at stage-level 
    player.stage.set("condition", game.treatment.networkCondition);

    // set initial network structure
    player.stage.set("hearsFrom", player.round.get("hearsFrom"));

    // fill current_data if mean_extreme or polarize treatment
    if(game.treatment.networkCondition == "mean_extreme" || game.treatment.networkCondition == "polarize"){
      current_data.push({id: i+1, value: (_.last(player.round.get("predictions")))})
    }
  });

  // If it's the initial stage or a 'static' network, proceed no further
  if (stage.name == "initial" || game.treatment.networkCondition == "static"){
    return;
  }

  // If networkCondition == "scheduled"...
  if (game.treatment.networkCondition == "scheduled"){

    // Grab network json objects from scheduled_networks array
    const snet1 = scheduled_networks[0];
    const snet2 = scheduled_networks[1];
    const snet3 = scheduled_networks[2];
    const snet4 = scheduled_networks[3];

    // If it's not the initial stage, then manipulate the 'hearsFrom' variable
    if (stage.name == "social1"){
      // for each player...
      game.players.forEach((player, i) => {

        // set sNeighbs1 containing the IDs of the player's neighbors
        var sNeighbs1 = snet1.links.filter(s=>s.target===player.get("id")).map(s=>s.source);

        // set hearsFrom as sNeighbs2
        player.stage.set("hearsFrom", sNeighbs1); 
      });
    };

    if (stage.name == "social2"){
      // for each player...
      game.players.forEach((player, i) => {

        // set sNeighbs1 containing the IDs of the player's neighbors
        var sNeighbs2 = snet2.links.filter(s=>s.target===player.get("id")).map(s=>s.source);

        // set hearsFrom as sNeighbs2
        player.stage.set("hearsFrom", sNeighbs2); 
      });
    };

    if (stage.name == "social3"){
      // for each player...
      game.players.forEach((player, i) => {

        // set sNeighbs1 containing the IDs of the player's neighbors
        var sNeighbs3 = snet3.links.filter(s=>s.target===player.get("id")).map(s=>s.source);

        // set hearsFrom as sNeighbs2
        player.stage.set("hearsFrom", sNeighbs3); 
      });
    };

    if (stage.name == "final"){
      // for each player...
      game.players.forEach((player, i) => {

        // set sNeighbs1 containing the IDs of the player's neighbors
        var sNeighbs4 = snet4.links.filter(s=>s.target===player.get("id")).map(s=>s.source);

        // set hearsFrom as sNeighbs2
        player.stage.set("hearsFrom", sNeighbs4); 
      });
    };
  };

  // If networkCondition == "mean_extreme"...
  if (game.treatment.networkCondition == "mean_extreme" && stage.name != "initial"){

    let meVals = []; // empty array to store the predictions of the just-finished stage
    let send = []; // empty array to store the ID of the 'mean-extreme' player to be broadcasted
    let receive = []; // empty array to store the IDs of players who will receive the 'mean-extreme' player
  
    // push values into meVals array
    current_data.forEach((item) => {
      meVals.push(item.value);
    });
    
    // as long as not everyone as the same prediction, do calculations
    if (meVals.length > 0 && !meVals.every((val) => val === meVals[0])){
  
      // calculate sum of players' predictions
      let sum = 0;
      for(let i in meVals) {
          sum += meVals[i];
      };
  
      // Calculate the meanPrediction 
      const meanPrediction = sum / meVals.length;

      // shuffle + sort data by value, descending
      current_data = _.shuffle(current_data)
      current_data = _.sortBy(current_data, 'value')

      // depending on mean prediction being over/under 50%,
      // push IDs into send and receive arrays
      if(meanPrediction > 50){
        send.push(current_data[game.treatment.playerCount-1].id);
        receive.push(current_data[0].id, current_data[1].id, current_data[2].id);
      }

      if(meanPrediction < 50){
        send.push(current_data[0].id);
        receive.push(current_data[game.treatment.playerCount-1].id, 
                     current_data[game.treatment.playerCount-2].id, 
                     current_data[game.treatment.playerCount-3].id);
      }

      if(meanPrediction == 50){
        return;
      }

    // make `send` a number to be pushed  
    send = Number(send);

    // Record IDs of players implicated in mean-extreme rewiring at stage-level
    stage.set("meSend", send);
    stage.set("meReceive", receive);
    //console.log("Sending", send, "to", receive);

    // loop through and rewire by manipulating players' 'hearsFrom' array
    game.players.forEach((player, i) => {

      // if estimates are not all equal, rewire accordingly...
      if (!meVals.every((val) => val === meVals[0])) {

        if (
          // if the player is in 'receive' array...
          receive.includes(i+1) &&
          // and isn't already connected to the 'send' player...
          !player.round.get("hearsFrom").includes(send)
        ) {
        
          const newNeighbors = player.round.get("hearsFrom");
  
          // then add highest to the player's hearsFrom array
          newNeighbors.unshift(send);
          player.stage.set("hearsFrom", newNeighbors);
        }
      };
    });
    }; 
  };

  // If networkCondition == "polarize"...
  if (game.treatment.networkCondition == "polarize"  && stage.name != "initial"){

    // shuffle + sort data by value, descending
    current_data = _.shuffle(current_data)
    current_data = _.sortBy(current_data, 'value')

    let polVals = [];
    current_data.forEach((item) => {
      polVals.push(item.value);
    });

    // if everyone has the same estimate, proceed no further...
    if (polVals.every((val) => val === polVals[0])){
      return;
    }

    let upperSend = current_data[game.treatment.playerCount-1].id;
    let lowerSend = current_data[0].id;

    var half = Math.floor(game.treatment.playerCount/2);
    let upperReceive = [current_data[half].id, current_data[half+1].id];
    let lowerReceive = [current_data[half-1].id, current_data[half-2].id];

    // Record IDs of players implicated in rewiring at stage-level
    stage.set("lowerSend", lowerSend);
    stage.set("upperSend", upperSend);
    stage.set("lowerReceive", lowerReceive);
    stage.set("upperReceive", upperReceive);

    //console.log("Send", upperSend, "to", upperReceive);
    //console.log("Send", lowerSend, "to", lowerReceive);

    // For each player... see if they're implicated in rewiring and push new neighbors' IDs into 'hearsFrom' array if so
    game.players.forEach((player, i) => {

      if (
        // and the player's id is in 'lowerReceive' array...
        lowerReceive.includes(i+1) &&
        // and isn't already connected to the 'lowerSend' player...
        !player.round.get("hearsFrom").includes(lowerSend)
      ) {
        const newNeighbors = player.round.get("hearsFrom");

        // then add lowerSend to the player's hearsFrom array
        newNeighbors.unshift(lowerSend);
        player.stage.set("hearsFrom", newNeighbors);
      }

      if (
        // and the player's id in 'upperReceive' array...
        upperReceive.includes(i+1) &&
        // and isn't already connected to the 'upperSend' player(s)...
        !player.round.get("hearsFrom").includes(upperSend)
      ) {
        const newNeighbors = player.round.get("hearsFrom");

        // then add highest to the player's hearsFrom array
        newNeighbors.unshift(upperSend);
        player.stage.set("hearsFrom", newNeighbors);
      }

      //  if player is upperSend or lowerSend...
      if (upperSend == i+1 || lowerSend == i+1) {
        // then cut all incoming links to player
        player.stage.set("hearsFrom", []);
      }
    });
  }; 
});

// onStageEnd is triggered after each stage.
// It receives the same options as onRoundEnd, and the stage that just ended.
Empirica.onStageEnd((game, round, stage) => {

  // loop through players, get latest stage prediction, and push it into round-level predictions array
  game.players.forEach((player, i) => {

    // get array of player's predictions for current round
    let predictions = player.round.get("predictions");
    
    // get array of player's rationales for current round
    let rationales = player.round.get("rationales");

    // get player's prediction from just-finished stage
    let latestPrediction = player.stage.get("value");

    // get player's rationale from just-finished stage
    let latestRationale = player.stage.get("rationale");

    // if player didn't submit a prediction in just-finished stage,then push a 50 into the round-level prediction array,
    // otherwise, push prediction from just-finished stage
    switch(latestPrediction){
      case undefined:
        predictions.push(50);
        break;
      default:
        predictions.push(player.stage.get("value"));
    }
    
    // same as above, but with rationale instead of prediction
    switch(latestRationale){
      case undefined:
        rationales.push(" ");
        break;
      default:
        rationales.push(player.stage.get("rationale"));
    }

    // re-set round-level array with updated values
    player.round.set("predictions", predictions);
    player.round.set("rationales", rationales);
  });
});

// onRoundEnd is triggered after each round.
// It receives the same options as onGameEnd, and the round that just ended.
Empirica.onRoundEnd((game, round) => {});

// onGameEnd is triggered when the game ends.
// It receives the same options as onGameStart.
Empirica.onGameEnd(game => {});

// ===========================================================================
// => onSet, onAppend and onChange ==========================================
// ===========================================================================

// onSet, onAppend and onChange are called on every single update made by all
// players in each game, so they can rapidly become quite expensive and have
// the potential to slow down the app. Use wisely.
//
// It is very useful to be able to react to each update a user makes. Try
// nontheless to limit the amount of computations and database saves (.set)
// done in these callbacks. You can also try to limit the amount of calls to
// set() and append() you make (avoid calling them on a continuous drag of a
// slider for example) and inside these callbacks use the `key` argument at the
// very beginning of the callback to filter out which keys your need to run
// logic against.
//
// If you are not using these callbacks, comment them out so the system does
// not call them for nothing.

// // onSet is called when the experiment code call the .set() method
// // on games, rounds, stages, players, playerRounds or playerStages.
// Empirica.onSet((
//   game,
//   round,
//   stage,
//   player, // Player who made the change
//   target, // Object on which the change was made (eg. player.set() => player)
//   targetType, // Type of object on which the change was made (eg. player.set() => "player")
//   key, // Key of changed value (e.g. player.set("score", 1) => "score")
//   value, // New value
//   prevValue // Previous value
// ) => {
//   // // Example filtering
//   // if (key !== "value") {
//   //   return;
//   // }
// });

// // onAppend is called when the experiment code call the `.append()` method
// // on games, rounds, stages, players, playerRounds or playerStages.
// Empirica.onAppend((
//   game,
//   round,
//   stage,
//   player, // Player who made the change
//   target, // Object on which the change was made (eg. player.set() => player)
//   targetType, // Type of object on which the change was made (eg. player.set() => "player")
//   key, // Key of changed value (e.g. player.set("score", 1) => "score")
//   value, // New value
//   prevValue // Previous value
// ) => {
//   // Note: `value` is the single last value (e.g 0.2), while `prevValue` will
//   //       be an array of the previsous valued (e.g. [0.3, 0.4, 0.65]).
// });

// // onChange is called when the experiment code call the `.set()` or the
// // `.append()` method on games, rounds, stages, players, playerRounds or
// // playerStages.
// Empirica.onChange((
//   game,
//   round,
//   stage,
//   player, // Player who made the change
//   target, // Object on which the change was made (eg. player.set() => player)
//   targetType, // Type of object on which the change was made (eg. player.set() => "player")
//   key, // Key of changed value (e.g. player.set("score", 1) => "score")
//   value, // New value
//   prevValue, // Previous value
//   isAppend // True if the change was an append, false if it was a set
// ) => {
//   // `onChange` is useful to run server-side logic for any user interaction.
//   // Note the extra isAppend boolean that will allow to differenciate sets and
//   // appends.
//    Game.set("lastChangeAt", new Date().toString())
// });

// // onSubmit is called when the player submits a stage.
// Empirica.onSubmit((
//   game,
//   round,
//   stage,
//   player // Player who submitted
// ) => {
// });