import React from 'react';
import { FlatList, StyleSheet, Text, View } from 'react-native';

export default class Playlist extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            items: [],
        };
    }

    render() {
        return (
            <FlatList
                data={this.state.items}
                renderItem={({ item }) =>
                    <View style={styles.item}>
                        <Text>{item.name}</Text>
                    </View>
                }
            />
        );
    }

    componentWillReceiveProps(nextProps) {
        this.updatePlaylist(nextProps.playlist);
    }

    componentDidMount() {
        this.updatePlaylist(this.props.playlist);
    }

    updatePlaylist(playlist) {
        this.setState({
            items: playlist.map((song) => {
                return {
                    key: song.id,
                    name: song.name
                };
            }),
        });
    }
}

const styles = StyleSheet.create({
    item: {
        height: 48,
        paddingTop: 8,
        paddingRight: 12,
        paddingBottom: 8,
        paddingLeft: 12,
        flexDirection: 'row',
        alignItems: 'center',
        borderBottomWidth: 1,
        borderBottomColor: 'lightgray',
    },
});
