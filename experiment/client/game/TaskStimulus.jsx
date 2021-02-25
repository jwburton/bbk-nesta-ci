import React from "react";

export default class TaskStimulus extends React.Component {
  render() {const { round, stage, player } = this.props;
  const question = round.get('question');   //retrieving this round's question
  
  // prompt for revision stages
  if(this.props.stage.name ==='social1' | this.props.stage.name ==='social2' | this.props.stage.name ==='social3')
   return (    
      <div className="task-stimulus">
        <i>
           View the responses of your network neighbors on the right and revise your prediction! 
        </i>
       <h2>{question}</h2>                       
    </div>
  );

  // prompt for final prediction stage
  if(this.props.stage.name ==='final')
   return (    
      <div className="task-stimulus">
       <i>
          Final prediction!  
       </i>
       <h2>{question}</h2>                       
    </div>
  );

  // prompt for initial predcition stage
  if(this.props.stage.name =='initial')
  return (
    <div className="task-stimulus">
        <h2>{question}</h2>                       
    </div>
  );
 }
}