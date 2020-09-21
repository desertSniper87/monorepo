#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 19.01.2019
# Last Modified Date: 25.01.2019
import pprint

from queue import PriorityQueue


num_of_tests = int(input())


# f = open("input429_3.txt")
# num_of_tests = int(f.readline())

def main():
    for _ in range(num_of_tests):
        graph = {}
        while True:
            # stripped_line = f.readline().strip()
            stripped_line = input().strip()
            # # print(f'{stripped_line}')
            if len(graph) == 0:
                # Initial Case: The graph is empty. Initial assignment
                graph[stripped_line] = []

            elif stripped_line == '*':
                # Go to output.
                break

            else:
                # Put the key in the dictionary.
                graph[stripped_line] = []

                for i in graph.keys():
                    if len(stripped_line) == len(i) and i != stripped_line:
                        distance = levensthein(stripped_line, i)
                        if distance == 1:
                            graph[stripped_line].append(i)
                            graph[i].append(stripped_line)

            if stripped_line == '*':
                break


        while True:
            # stripped_line = f.readline().strip()

            stripped_line = input().strip()

            if stripped_line == "":
                break

            elif '#' in stripped_line:
                continue

            input_words = stripped_line.split(' ')

            # word#1 is the word we want to find
            traverse_distance = 0

            source = input_words[0]
            target = input_words[1]

            dist = dijkstra(graph, source, target)
            print(source, target, dist)

            # for i in graph[input_words[0]]:
                # # print(levensthein(input_words[1], i), i)


def dijkstra(graph: dict, source: str, dest: str) -> int:
    """
    :return: total cost
    """

    # print(f'source: {source}\n'
    #       f'destination: {dest}')

    # del graph['']
    # print('Graph:')
    # pprint.pprint(graph)


    distance_from_source = {}
    for v in graph:
        distance_from_source[v] = float('inf')

    q = PriorityQueue()
    explored = set()

    distance_from_source[source] = 0
    q.put(source)
    while not q.empty():
        # for e in list(q.queue):
        #     print(e)


        front = q.get()
        explored.add(front)

        # print(front_v)

        for neighbour in graph[front]:
            if neighbour not in explored:
                q.put(neighbour)
        for nbour in graph[front]:
            if distance_from_source[nbour] > distance_from_source[front] + 1:
                distance_from_source[nbour] = distance_from_source[front] + 1

    return distance_from_source[dest]





def remove_visited(adj_list: list, visited: list) -> list:
    """

    :param adj_list:
    :param visited:
    :return:
    """
    for i in visited:
        try:
            adj_list.remove(i)
        except ValueError:
            pass

    return  adj_list


def take_shortcut(path: list) -> int:
    shortcut = 0

    try:
        for idx, i in enumerate(path):
            if levensthein(i, path[idx-1]) == levensthein(i, path[idx+1]):
                shortcut += 1

    except (IndexError):
        shortcut += 1
        # print(f'End')
        pass

    except ValueError:
        shortcut += 1


    return shortcut


def traverse_for_best_match(target_string: str, adj_list: list) -> str:
    """TODO: Docstring for traverse_for_best_match.
    :param target_string: Is the matching string.
    :param adj_list: adjacency list of the string beside the target string.

    This function finds out the best match in the list and returns it.


    """
    # # print(adj_list)
    if len(adj_list) == 1:
        return adj_list[0]

    else:
        return min(adj_list, key=lambda x: levensthein(target_string, x))



def levensthein(string1: str, string2: str) -> int :
    """TODO: Docstring for levensthein.

    :string1: str: TODO
    :string2: str: TODO
    :returns: TODO

    """
    # # print(f'levensthein')
    if len(string1) == len(string2):
        l = len(string1)
    else:
        raise NotImplemented
        # To be implemented

    distance = 0
    for i in range (l):
        if string1[i] != string2[i]:
            distance += 1

    return distance            


if __name__ == "__main__":
    main()
