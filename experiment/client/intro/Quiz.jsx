import React from "react";

import { Centered } from "meteor/empirica:core";

const Radio = ({ selected, name, value, label, onChange }) => (
  <label>
    <input
      type="radio"
      name={name}
      value={value}
      checked={selected === value}
      onChange={onChange}
    />
    {label}
  </label>
);

export default class Quiz extends React.Component {
  state = { 
    certainty: "",
    nRounds: "", 
    incentives: ""
  };

  handleChange = event => {
    const el = event.currentTarget;
    this.setState({ [el.name]: el.value.trim().toLowerCase() });
  };

  handleSubmit = event => {
    event.preventDefault();

    if (
      this.state.certainty !== "100" || 
      this.state.nRounds !== "10" || 
      this.state.stages !== "both"){
      alert("Sorry, you have one or more mistakes. Please read the instructions and try again.");
    } else {
      this.props.onNext();
    }
  };

  
  render() {
    const { hasPrev, hasNext, onNext, onPrev } = this.props;
    const { certainty, nRounds, stages } = this.state;
    return (
      <Centered>
        <div className="quiz">
        <h1> Quiz </h1>
        <form onSubmit={this.handleSubmit}>
          <div>
            <label htmlFor="certainty">
              <b>If you are completely certain that an event will occur, what should your prediction be? (hint: just the numeric value with no % sign) 
                </b>
              </label>
              <input
                  id="certainty"
                  type="number"
                  min="0"
                  max="100"
                  step="1"
                  dir="auto"
                  name="certainty"
                  value={certainty}
                  onChange={this.handleChange}
                />
          </div>
          <br />

          <div>
            <label htmlFor="nRounds">
              <b>How many rounds will there be in the study?
                </b>
              </label>
              <input
                  id="nRounds"
                  type="number"
                  min="0"
                  max="150"
                  step="1"
                  dir="auto"
                  name="nRounds"
                  value={nRounds}
                  onChange={this.handleChange}
                />
          </div>
          <br />

            <label htmlFor="stages">
              <b>What should you do during the second, third, fourth, and fifth stages of each round?</b>
            </label>
            <div>
              <Radio
                selected={stages}
                name="stages"
                value="revise"
                label="Look at your network neighbors' responses and revise your prediction"
                onChange={this.handleChange}
              />
              <Radio
                selected={stages}
                name="stages"
                value="write"
                label="Write a rationale for your prediction"
                onChange={this.handleChange}
              />
              <Radio
                selected={stages}
                name="stages"
                value="both"
                label="Both of the above"
                onChange={this.handleChange}
              />                
            </div>
  
            <p>
               <button type="button" onClick={onPrev} disabled={!hasPrev}>
                Back to instructions
              </button> 
              <button type="submit">Begin the study</button>
            </p>
          </form>
        </div>
      </Centered>
    );
  }
}
