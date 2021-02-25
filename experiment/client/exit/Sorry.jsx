import React from "react";

import { Centered } from "meteor/empirica:core";
import { Button } from "@blueprintjs/core";

export default class Sorry extends React.Component {
  static stepName = "Sorry";

  render() {
    const { player, hasNext, onSubmit } = this.props;
    let msg;
    switch (player.exitStatus) {
      case "gameFull":
        msg = "The game was overbooked to ensure enough players showed up, and the game is now full.";
        break;
      case "gameLobbyTimedOut":
        msg = "There were not enough players for the game to start. This is because some MTurkers who signed up did not show up.";
        break;
      // case "playerLobbyTimedOut":
      //   msg = "???";
      //   break;
      case "playerEndedLobbyWait":
        msg =
          "You decided to stop waiting. We are sorry it was too long a wait.";
        break;
      default:
        msg = "Unfortunately the game has been cancelled. This could be due to a server error, or because players were behaving inapproriately. Please submit 'game-cancelled' as the secret code to be paid for your time.";
        break;
    }

    return (
      <Centered>
        <div className="score">
          <h1>Sorry!</h1>

          <p>{msg}</p>

          {/*{player.exitStatus !== "gameFull" ? (*/}

          {/*<p>*/}
          {/*Please return the HIT now so our platform does register your MTurk.*/}
          {/*Please come back for one of the next batches of Part 1. We will submit new*/}
          {/*batches on Monday the 6th of August and Tuesday the 7th of August*/}
          {/*(batches of 100 games every hour starting at 2PM ET until 5PM).*/}
          {/*</p>*/}

          {player.exitStatus === "gameLobbyTimedOut" ? (
            <p>
              Please submit <strong>lobbytimeout</strong> as the secret code in order to
              receive an extra bonus for your time.
            </p>
          ) : null}

          {player.exitStatus === "gameFull" ? (
            <p>
              Please submit <strong>FZgameFullCSOP213093</strong> as the survey code in
              order to receive $0.50 for showing up and reading the instructions.
            </p>
          ) : null}

          {/*) : (*/}
          {/*<p>*/}
          {/*Please click on: <strong>Reset current session</strong> from the*/}
          {/*top right side of the page (if it appears for you) to see if there*/}
          {/*are other games that you could join now. Note you will need to go*/}
          {/*over the instructions and quiz again (they might be different for*/}
          {/*different games). Otherwise, Please return the HIT now so our*/}
          {/*platform does not register your MTurk ID as someone who already*/}
          {/*participated.*/}
          {/*</p>*/}

          <p>
            {" "}
            {/*We will send an email notification once the next  HIT is scheduled.*/}
          </p>

          {/*This is not really needed .. all of these people failed to start the game .. if we allow them to submit, then their data will be deleted, we don't want that*/}
          <p>
            {/*{hasNext ? (*/}
              {/*<Button*/}
                {/*intent={"primary"}*/}
                {/*type="button"*/}
                {/*onClick={() => onSubmit()}*/}
              {/*>*/}
                {/*Done*/}
              {/*</Button>*/}
            {/*) : (*/}
              {/*""*/}
            {/*)}*/}
          </p>
        </div>
      </Centered>
    );
  }
}