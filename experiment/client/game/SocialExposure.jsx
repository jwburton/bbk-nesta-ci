import React from "react";
import Slider from "meteor/empirica:slider";

export default class SocialExposure extends React.Component {
  renderSocialInteraction(otherPlayer) {
    
    return (
      <div key={otherPlayer._id}>
        <img src={otherPlayer.get("avatar")} className="profile-avatar" />
        <div className="range">
          <Slider
          min={0}
          max={100}
          stepSize={1}
          labelStepSize={50}
          value={otherPlayer.round.get("predictions")[otherPlayer.round.get("predictions").length-1]} 
          labelRenderer={num=>(num)+'%'} 
          disabled
          hideHandleOnEmpty
          />
        </div>
        <div style={{marginTop:'15px'}}>
        Player {otherPlayer.get('id')} said: {otherPlayer.round.get("rationales")[otherPlayer.round.get("rationales").length-1]}
        </div>


      </div>
    );
  }

  render() {
    const { game, player } = this.props;
    const neighbours = game.players.filter(p => player.stage.get('hearsFrom').includes(p.get('id')));

    return (
      <div className="social-exposure">
        <p>
          <strong>You currently have {neighbours.length} network neighbor(s):</strong>
        </p>
        {neighbours.map(p => this.renderSocialInteraction(p))}
      </div>
    );
  }
}