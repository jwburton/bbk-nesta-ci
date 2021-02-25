import React from "react";

import { Centered } from "meteor/empirica:core";

export default class ExitSurvey extends React.Component {
  static stepName = "ExitSurvey";
  state = { 
    age: "", 
    gender: "", 
    familiarity: "",  
    feedback: "" 
  };

  handleChange = event => {
    const el = event.currentTarget;
    this.setState({ [el.name]: el.value });
  };

  handleSubmit = event => {
    event.preventDefault();
    this.props.onSubmit(this.state);
  };

  render() {
    const { player } = this.props;
    const { age, gender, familiarity, feedback } = this.state;

    return (
      <Centered>
        <div className="exit-survey">
          <h1> Exit survey </h1>

              <div>
                <label htmlFor="feedback">
                <b>Any feedback, including problems you encountered.</b>
                </label>
                <div>
                  <textarea
                  type="text"
                    dir="auto"
                    id="feedback"
                    name="feedback"
                    value={feedback}
                    cols={80}
                    rows={3}
                    placeholder="E.g., The user interface lagged."
                    onChange={this.handleChange}
                  />
                </div>
              </div>
              
            </div>

          <form onSubmit={this.handleSubmit}>
            <div className="form-line">
              <div>
                <label htmlFor="age"><b>Age</b></label>
                <div>
                  <input
                    id="age"
                    type="number"
                    min="0"
                    max="150"
                    step="1"
                    dir="auto"
                    name="age"
                    value={age}
                    onChange={this.handleChange}
                    required
                  />
                </div>
              </div>
              <br />

              <div>
                <label htmlFor="gender"><b>Gender</b></label>
                <div>
                  <input
                    id="gender"
                    type="text"
                    dir="auto"
                    name="gender"
                    value={gender}
                    onChange={this.handleChange}
                    autoComplete="off"
                    required
                  />
                </div>
              </div>
              <br />

            </div>

            

            <button type="submit">Submit</button>
          </form>
      </Centered>
    );
  }
}
