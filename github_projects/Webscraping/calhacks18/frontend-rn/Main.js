import React from 'react';
import { Button, StyleSheet, View, WebView } from 'react-native';
import { connect } from 'react-redux';

import { ENDPOINT_BASE } from './constants.js';
import { setUser } from './reducer.js';
import MoodPickerTab from './MoodPickerTab.js';
import SentimentTab from './SentimentTab.js';

const TABS = {
    MOOD_PICKER: 'mood-picker',
    SENTIMENT: 'sentiment',
};

class Main extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            tab: TABS.MOOD_PICKER,
        };
    }

    renderTabView() {
        switch (this.state.tab) {
            case TABS.MOOD_PICKER: return <MoodPickerTab />;
            case TABS.SENTIMENT: return <SentimentTab />;
        }
    }

    renderAuthView() {
        const webViewProps = {
            ref: 'webview',
            onMessage: (event) => {
                this.props.setUser(event.nativeEvent.data);
            },
            javaScriptEnabled: true,
            source: {
                uri: `${ENDPOINT_BASE}/spotify/auth`,
            }
        }

        return <WebView {...webViewProps} style={{ flex: 1 }} />;
    }

    renderInvisibleLogoutView() {
        const webViewProps = {
            source: {
                uri: `${ENDPOINT_BASE}/spotify/logout`,
            }
        }

        return (
            <View style={{ width: 0, height: 0, flex: 0 }}>
                <WebView {...webViewProps} />
            </View>
        );
    }

    render() {
        if (!this.props.userId) {
            return (
                <View style={styles.container}>
                    <View style={styles.statusBar}></View>
                    {this.renderAuthView()}
                </View>
            )
        }
        return (
            <View style={styles.container}>
                <View style={styles.statusBar}></View>
                <View style={styles.header}>
                    <Button
                        title="Mood Slider"
                        onPress={() => this.setState({ tab: TABS.MOOD_PICKER })}
                    />
                    <Button
                        title="Song from your Diary"
                        onPress={() => this.setState({ tab: TABS.SENTIMENT })}
                    />
                </View>
                {this.props.userId ? this.renderTabView() : null}
                {this.renderInvisibleLogoutView()}
            </View>
        );
    }
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
        backgroundColor: '#fff',
    },
    statusBar: {
        height: 20,
    },
    header: {
        height: 60,
        flexDirection: 'row',
        justifyContent: 'space-evenly',
        alignItems: 'center',
        borderBottomWidth: 1,
        borderBottomColor: 'gray',
    },
});

const mapStateToProps = (state) => {
    return {
        userId: state.userId
    };
};

const mapDispatchToProps = (dispatch) => {
    return {
        setUser(userId) {
            dispatch(setUser(userId));
        }
    };
};

export default connect(mapStateToProps, mapDispatchToProps)(Main);
