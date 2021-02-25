import React from "react";

import { Centered, ConsentButton } from "meteor/empirica:core";

export default class Consent extends React.Component {
  render() {
    return (
      <Centered>
        <div className="consent">

        <h2> About this study </h2>
        <p><ul>
        <li>This study will take 1 hour and involves a collaborative prediction game with other MTurkers.</li>
        <li>The game is overbooked to ensure enough people arrive (many MTurkers do not show up despite signing up).</li> 
        <li>If the study fills up before you get to the waiting room, you won't get to play.</li>
        <li>You will be paid $0.50 for reading through the instructions, even if you don't get to play.</li>
        <li>Those who do play will be paid an additional $6.75 (for a total of $7.25), and get a chance at a performance bonus.</li>
        </ul> </p>


        <h2> What you need to do </h2>
        <p><ul>
        <li>Please use a computer, not a mobile device. </li>
        <li>When the game starts, please give it your full attention.</li>
        <li><b>If you don't play actively and provide responses, you may lose your payment.</b></li>
        </ul></p>


        <h2> Consent to participate </h2>
        <p><i>
          This study is part of a Birkbeck, University of London scientific project. Your decision
          to participate in this experiment is entirely voluntary. There are
          no known or anticipated risks to participating in this experiment.
          The only information we will keep, in addition to your responses, is the timestamps of your
          interactions with our site. The results of our research may be
          presented at scientific meetings or published in scientific
          journals. 
        </i></p>
          <p><i>
            Clicking on the "AGREE" button indicates that you are at
            least 18 years of age, and agree to participate voluntary.
            </i></p>
          <br />
          <ConsentButton text="I AGREE" />
        </div>
      </Centered>
    );
  }
}
