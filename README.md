# AI-projects
Projects implemented with AI algortihms developed with prolog

## Domino 1v1 Player

A program that chooses the best move in a 1v1 domino game based on the pieces it has, the pieces played and the ohter player moves. 

It uses alpha-beta prunning with a modifiable depth. 

The heuristic gives the highest value to a winning position and the lowest to a losing one. It gives the next highest positive value to closing a side of the game for the opponent and the next one to geting rid of a double digit piece in the next move. It also gives a positive/negative value to a more/less diverse set of numbers after the move.

<img align="center" alt="Coding" width="400" src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/AB_pruning.svg/400px-AB_pruning.svg.png">

## Soccer Calendar Generator

This program uses and evolutionary algorithm to find an equilibrated calendar that works. It is considered finished when a certain heuristic value is reached.
It uses a merge and mutation function to change the population and create a new one. Each cycle the population duplicates and the half with the highest values remain.


## Polynomial operations

A program that solves basic polynomial operations as addition/difference, evaluation, multplication divition, derivative and composition. 
It uses dynamic knowledge database to store multiple polynomials and have access to them all.
