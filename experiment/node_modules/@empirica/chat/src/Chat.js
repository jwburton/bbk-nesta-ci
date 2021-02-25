import PropTypes from "prop-types";
import React from "react";
import Footer from "./Footer";
import Message from "./Message";
import Messages from "./Messages";
import ErrorBoundary from "./ErrorBoundary";
import { rest } from "lodash";

export default class Chat extends React.PureComponent {
  state = { isOpen: true };

  onClick = () => {
    this.setState({ isOpen: !this.state.isOpen });
  };

  onNewMessage = (msg) => {
    const { onNewMessage, scope, customKey } = this.props;

    if (onNewMessage) {
      msg = onNewMessage(msg);
      if (!msg) {
        return;
      }
    }

    scope.append(customKey, msg);
  };

  componentDidMount = () => {
    const { dockStartOpen, docked } = this.props;
    if (docked && !dockStartOpen) {
      this.setState({
        isOpen: false,
      });
    }
  };

  render() {
    const { isOpen } = this.state;
    const {
      player,
      scope,
      customKey,
      customClassName,
      docked,

      filter,
      timeStamp,

      header: HeaderComp,
      message: MessageComp,
      footer: FooterComp,
      ...rest
    } = this.props;

    const common = { player, scope, customKey };

    return (
      <ErrorBoundary>
        <div
          className={`${
            customClassName ? customClassName : "empirica-chat-container"
          } ${docked ? "docked" : "undocked"}`}
        >
          <div className={`chat ${isOpen ? "open" : ""}`}>
            {docked && (
              <HeaderComp {...common} onClick={this.onClick} isOpen={isOpen} />
            )}
            {isOpen ? (
              <>
                <Messages
                  {...common}
                  messageComp={MessageComp}
                  filter={filter}
                  {...rest}
                />
                <FooterComp
                  {...common}
                  timeStamp={timeStamp}
                  onNewMessage={this.onNewMessage}
                />
              </>
            ) : (
              ""
            )}
          </div>
        </div>
      </ErrorBoundary>
    );
  }
}

Chat.defaultProps = {
  customKey: "chat",
  docked: false,
  customClassName: "",
  dockStartOpen: true,
  hideAvatar: false,
  hideName: false,
  svgAvatar: false,
  header: ({ onClick, isOpen }) => (
    <div className="header">
      <span className="title">CHAT</span>
      <span className="close-button" onClick={onClick}>
        {isOpen ? "-" : "+"}
      </span>
    </div>
  ),
  message: Message,
  footer: Footer,
};

Chat.propTypes = {
  player: PropTypes.object.isRequired,
  scope: PropTypes.object.isRequired,
  customKey: PropTypes.string.isRequired,
  timeStamp: PropTypes.instanceOf(Date),
  docked: PropTypes.bool,
  dockStartOpen: PropTypes.bool,
  hideAvatar: PropTypes.bool,
  hideName: PropTypes.bool,
  svgAvatar: PropTypes.bool,
  customClassName: PropTypes.string,

  onNewMessage: PropTypes.func,
  filter: PropTypes.func,

  header: PropTypes.elementType.isRequired,
  message: PropTypes.elementType.isRequired,
  footer: PropTypes.elementType.isRequired,
};
