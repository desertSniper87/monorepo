from datetime import datetime
from django.contrib.auth.models import User
from django.db import models

class Order(models.Model):
    user = models.ForeignKey(User, null=True, blank=True, on_delete=models.CASCADE)
    carts = models.ManyToManyField('carts.Cart', blank=True)
    order_total = models.DecimalField(default=0.00, max_digits=100, decimal_places=2)
    paid = models.BooleanField(default=False)
    timestamp = models.DateTimeField(default=datetime.now)


    # def save(self, *args, **kwargs):
    #     return super(Order, self).save(*args, **kwargs)


    def __str__(self):
        return self.id

    @property
    def get_seller(self):
        pass

# def post_save_cart_total(sender, instance, created, *args, **kwargs):
#     if not created:  # The sender is Cart. Cart pre-exist. So we find the cart.
#         cart_obj = instance
#         cart_total = cart_obj.total
#         cart_id = cart_obj.id
#
#         qs = Order.objects.filter(cart__id=cart_id)
#
#         if qs.count() == 1:
#             order_obj = qs.first()
#             order_obj.update_total()
#
#
#
#
# post_save.connect(post_save_cart_total, sender=Cart)
#
#
# def post_save_new_cart_total(sender, instance, created, *args, **kwargs):
#     if created:                          # No cart existed
#         instance.update_total()
#
#
# post_save.connect(post_save_new_cart_total, sender=Order)

