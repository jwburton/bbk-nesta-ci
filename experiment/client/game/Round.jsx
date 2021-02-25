import React from "react";

import PlayerProfile from "./PlayerProfile.jsx";
import SocialExposure from "./SocialExposure.jsx";
import Task from "./Task.jsx";

const roundSound = new Audio("sounds/round-sound.mp3");
const gameSound = new Audio("sounds/bell.mp3");

export default class Round extends React.Component {
  componentDidMount() {
    const { player, game } = this.props;
    if (game.get("justStarted")) {
      //play the bell sound only once when the game starts
      gameSound.play();
      game.set("justStarted", false);
    } else {
      roundSound.play();
    }
  }
  render() {
    const { round, stage, player, game } = this.props;

    let social = "";
    if(stage.name==="social1" | stage.name==="social2" | stage.name==="social3" | stage.name==="final"){
      social = <SocialExposure stage={stage} player={player} game={game} />
    }
    return (
      <div className="round">
        <div className="content">
          <PlayerProfile player={player} stage={stage} game={game} />
          <Task game={game} round={round} stage={stage} player={player} />
          {social}
        </div>
      </div>
    );
  }
}
