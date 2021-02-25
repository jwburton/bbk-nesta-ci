import React from "react";
import Slider from "meteor/empirica:slider";

export default class TaskResponse extends React.Component {
  handleChange = num => {
    const { player } = this.props;
    const value = Math.round(num * 100) / 100;
    player.stage.set("value", value);
  };

  handleSubmit = event => {
    event.preventDefault();

    const { player } = this.props;
    
    // player.set('lastValue', player.round.get("value"));
    // player.set('lastRationale', player.round.get("rationale"));

    const submitted = player.get('submitted') ?? [];
    submitted.push({stage: player.stage._id, value: player.stage.get("value"), rationale: player.stage.get("rationale")})
    player.set('submitted', submitted); 
    player.stage.submit();
  };

  // Once submitted, render this
  renderSubmitted() {
    return (
      <div className="task-response">
        <div className="response-submitted">
          <h5>Waiting on other players...</h5>
          Please wait until all players are ready
        </div>
      </div>
    );
  }


    // Slider formatting
  renderSlider() {
    const { player } = this.props;
    const value = player.stage.get("value");
    return (
      <Slider
      min={0}
      max={100}
      stepSize={1}
      labelStepSize={25}
      onChange={this.handleChange}
      value={value}
      labelRenderer={num=>(num)+'%'} 
      hideHandleOnEmpty
      required
      />
    );
  }

  handleInputChange = event => {
    const value = event.currentTarget.value;
    const { player } = this.props;
    player.stage.set("rationale", value);
  };

  // Text box formatting
  renderInput() {
    const { player } = this.props;
    const value = player.stage.get("rationale");
    return (
      <textarea 
       type="text"
        onChange={this.handleInputChange}
        value={value}
        cols={42}
        rows={3}
        required
      />
    );
  }

  // hide input fields if social exposure stage -- not using this feature
   render() {
     // if(this.props.stage.name ==='social1' | this.props.stage.name ==='social2' | this.props.stage.name ==='social3' | this.props.stage.name ==='final') return "";
    const { player } = this.props;

    // If the player already submitted, don't show the slider or submit button
    if (player.stage.submitted) {
      return this.renderSubmitted();
    }

    return (
      <div className="task-response">
        <form onSubmit={this.handleSubmit}>
        <b>Estimate the probability of the above event occuring:</b>
          {this.renderSlider()}
          <br />
          <br />
          <b>Provide a rationale for your estimate:</b>
          {this.renderInput()}
          {/* <input type='text'></input> */}
          <br />
          <button 
          type="submit">
            Submit
          </button>
        </form>
      </div>
    );
  }
}
