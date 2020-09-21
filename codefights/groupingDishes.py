dishes = [["Salad", "Tomato", "Cucumber", "Salad", "Sauce"],
            ["Pizza", "Tomato", "Sausage", "Sauce", "Dough"],
            ["Quesadilla", "Chicken", "Cheese", "Sauce"],
            ["Sandwich", "Salad", "Bread", "Tomato", "Cheese"]]

from collections import defaultdict

def groupingDishes(dishes):
    h =  defaultdict(list)

    for dish in dishes:
        for i in dish[1:]:
            h[i].append(dish[0])

    # print(h)
    result = []
    for i in sorted(h):
        if len(h[i])>1:
            print(i, h[i])
            r = []
            r.append(i)
            for j in sorted(h[i]):
                r.append(j)

            result.append(r)

    # print(result)
    return result



groupingDishes(dishes)



