// Credit goes to Gabehabe at Dream in Code
// http://www.dreamincode.net/forums/topic/57487-stl-queues/
//
// The story and names have been changed to protect the innocent.

#include <iostream>
#include <queue>
#include <string>

using namespace std;

int main ()
{

    queue <string> names; /* Declare a queue */
    names.push ("Ben"); /* Add some values to the queue */
    names.push ("Erin"); /* Much like vectors */
    names.push ("Dan"); /* This basically does the same thing */

    cout << "Welcome to US Coney and Cone!" << endl << endl;
    cout << "Now serving: "
         << names.front () << endl << endl;
    names.pop ();
    cout << "There are currently " << names.size ()
         << " people in the queue. "
         << "The next person in the queue is "
         << names.front () << "." << endl << endl

         << names.back () << " is the last person in the queue."
         << endl;

    cin.get ();
    return 0;

}
