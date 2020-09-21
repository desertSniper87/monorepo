import React from 'react';
import { Button, StyleSheet, Slider, View } from 'react-native';
import { connect } from 'react-redux';

import { ENDPOINT_BASE } from './constants.js';
import Playlist from './Playlist.js';

class MoodPickerTab extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            mood: 0,
            playlist: [],
        };
    }

    render() {
        return (
            <View style={styles.container}>
                <View style={styles.sliderContainer}>
                    <Slider
                        style={styles.slider}
                        selectedValue={this.state.mood}
                        minimumValue={-1}
                        maximumValue={1}
                        onSlidingComplete={(value) => {
                            this.setState({ mood: value }, () => this.fetchPlaylist());
                        }}
                    />
                    <Button
                        title="Rock Spotify"
                        onPress={() => this.rockSpotify()}
                    />
                </View>
                <Playlist styles={styles.playlist} playlist={this.state.playlist} />
            </View>
        );
    }

    componentDidMount() {
        this.fetchPlaylist();
    }

    async fetchPlaylist() {
        this.setState({ playlist: [] }); // Empty the playlist first

        const response = await fetch(`${ENDPOINT_BASE}/playlist/create/mood?mood=${this.state.mood}`, {
            headers: {
                'Authorization': this.props.userId,
            },
        });
        const playlist = await response.json();
        this.setState({ playlist });
    }

    async rockSpotify() {
        const response = await fetch(`${ENDPOINT_BASE}/playlist/create/mood?mood=${this.state.mood}&rock=1`, {
            headers: {
                'Authorization': this.props.userId,
            },
        });
        const message = await response.text();
        alert(message);
    }
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
    },
    sliderContainer: {
        height: 48,
        paddingRight: 16,
        paddingLeft: 16,
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
        borderBottomWidth: 1,
        borderBottomColor: 'gray',
    },
    slider: {
        flex: 1,
    },
    playlist: {
        flex: 1,
    },
});

const mapStateToProps = (state) => {
    return {
        userId: state.userId
    };
};

const mapDispatchToProps = (dispatch) => {
    return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(MoodPickerTab);
