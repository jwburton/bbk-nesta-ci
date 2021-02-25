import React from "react";

import { Centered } from "meteor/empirica:core";

export default class InstructionStepTwo extends React.Component {
  render() {
    const { hasPrev, hasNext, onNext, onPrev } = this.props;
    return (
      <Centered>
        <div className="instructions">
          <h1> Performance incentives </h1>

          <p>
          In addition the standard payment for your participation in this study, you can earn a bonus if you are in one of the 9 most accurate study groups.
          Groups' accuracy will be determined by taking the average final prediction of all participants in a study group for each round and averaging its collective error across all 10 rounds. 
          This means you are incentivized to work together by making accurate predictions and providing honest rationales for each of your predictions.
        </p>

        <p>
          The top three most accurate study groups will receive 2x pay (i.e., an additional $7.25 on top of the base $7.25 payment), the 4th- through 6th-placed study groups will
          receive 1.67x pay, and the 7th- through 9th-placed study groups will receive 1.33x pay. We will run no more than 40 study groups.
        </p>

        <p>
          Please note that it will take time before we can allocate these bonus payments because we must wait and 
          see if the events you predict do or do not occur in the real world in order to know which predictions were correct.
        </p>

          <p>
            <button type="button" onClick={onPrev} disabled={!hasPrev}>
              Previous
            </button>
            <button type="button" onClick={onNext} disabled={!hasNext}>
              Next
            </button>
          </p>
        </div>
      </Centered>
    );
  }
}
