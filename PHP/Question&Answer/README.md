# online-quiz-system
An online quiz system built on PHP, JS and HTML. It has inbuilt Timer support along with Admin Panel

#How to Use

1. Use the Admin Panel to set up quiz. Quiz won't be enabled unless you click the "Enable" button. Click on the same to enable an added quiz.
2. Scores are updated realtime on the server, however the leaderboard will be updated only when the user finishes the quiz, or there is a time out or the admin ends the quiz by clicking on "Disable" button.
3. Once the admin clicks on the disable button, the quiz ends for all the users taking that quiz, irrespective of their active or inactive state (whether logged in or left the quiz in the middle only). The leaderboard will be updated either when a user "Finishes" his /her quiz and when the admin "disables" the quiz. 
4. Once the quiz is disabled, the quiz becomes inaccessible. If the quiz is enabled again later, only those user who have not already taken the quiz can take the quiz.
5. It is recommended that you Enable the quiz when all the users are ready and disable the quiz when all the users have completed the quiz or time limit of taking the quiz has exceeded.

#Bugs:

1. Too many SQL queries, needs optimization. Yet not suitable for more than 200 simultaneous user.
2. Security issues, need to sanitize the URL queries.
