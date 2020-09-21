from django.contrib.auth.models import User, Group

from carts.models import Cart


# class UserMethods(User):
#     class Meta:
#         proxy = True
#     def get_cart_total(self):
#         total = 0
#         for cart in Cart.objects.filter(user=self.id):
#             total += cart.total
#
#         return total

    # def get_role(self):
    #     groups = self.groups.first()
    #     return Group.objects.get(id=self.id)
