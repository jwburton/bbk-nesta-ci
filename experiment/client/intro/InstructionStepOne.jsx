import React from "react";

import { Centered } from "meteor/empirica:core";

export default class InstructionStepOne extends React.Component {
  render() {
    const { hasPrev, hasNext, onNext, onPrev, game } = this.props;

    return (
      <Centered>
        <div className="instructions">
          <h1> Instructions </h1>

          <p>
          In this study you will be asked to estimate the probability of events occuring in the near future on a scale of 0-100%. On this scale, a response of 0 means that you 
          are absolutely certain an event <i>will not occur</i>, 100 means that you are absolutely certain an event <i>will occur</i>,
          and 50 means that you are entirely unsure whether the event will occur or not.
        </p>

        <p>
          Throughout the study you will be interacting in a network of 16 MTurkers in real time. Your objective is to make your prediction as
          accurate as possible and to help others by providing honest rationales (1-2 sentences) for your predictions. Your rationales should explain the reasoning behind your prediction. 
          For example, this could be "<i>I read an article that said...</i>", "<i>Another player says...</i>", or even "<i>I don't know, I'm guessing</i>".
        </p>
        
        <p>
          The study has <b>10 rounds</b>. In each round you will be making predictions about a near future event, and sharing your responses with other participants over the following <b>5 stages</b>:
        </p> 

          <ol> 
          <li><b>Initial Prediction:</b> 60 seconds to provide an initial prediction on the event in question and write a short rationale on your own. </li>
          
          <li><b>Revision 1:</b> 60 seconds to view other participants' predictions and rationales (while others view yours!), and then revise your responses.</li>
          
          <li><b>Revision 2:</b> 60 seconds to view others' revised responses and revise your own again.</li>
          
          <li><b>Revision 3:</b> Another 60 seconds to view others' revised responses and revise your own again.</li>

          <li><b>Final Prediction:</b> A final 60 seconds to view others' revised responses provide your final responses.</li>
          </ol>

        <p>
          At each stage of revision you will only be able to view the responses of some of the other participants (i.e., your <i>network neighbors</i>). Your network neighbors might change throughout the 
          the study, and at some stages you might not have any network neighbors, so it is important to pay attention to whose responses you are viewing. 
          Also note that the participants whose responses you view might not be able to see yours.
        </p>

        <p>
          The entire study will take you about 1 hour. 50 minutes for the actual study, plus the time it takes for other participants to arrive. 
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
