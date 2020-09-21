from _decimal import Decimal
from django.contrib.auth.models import User
from django.core.exceptions import ValidationError
from django.db import models
from django.db.models.signals import post_save, pre_save
from django.dispatch import receiver

from orders.models import Order
from products.models import Product


class Cart(models.Model):
    """Docstring for Cart. """
    user = models.ForeignKey(User, null=True, blank=True, on_delete=models.CASCADE) # buyer
    product = models.ForeignKey(Product, blank=True, null=True, on_delete=models.CASCADE)
    quantity = models.IntegerField(blank=True)
    total = models.DecimalField(default=0.00, max_digits=100, decimal_places=2)
    ordered = models.BooleanField(default=False)
    updated = models.DateTimeField(auto_now=True)
    timestamp = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return str(self.id)

    def save(self, delete_old_cart=True, *args, **kwargs):
        if delete_old_cart is True and self.product is not None:
            max_quantity = self.product.available_quantity
            try:
                prev_unordered_cart = Cart.objects.filter(user=self.user,
                                                          product=self.product,
                                                          ordered=False).first()
                if prev_unordered_cart is None:
                    self.clean_total()
                    return super(Cart, self).save(*args, **kwargs)
                elif prev_unordered_cart.id == self.id:
                    return
                elif prev_unordered_cart.quantity + self.quantity <= max_quantity:
                    self.quantity += prev_unordered_cart.quantity
                    # self.total += self.product.price_taka * self.quantity
                    prev_unordered_cart.delete()
                else:
                    raise ValidationError
            except Cart.DoesNotExist:
                pass

        self.clean_total()
        return super(Cart, self).save(*args, **kwargs)

    def clean_total(self):
        try:
            self.total += Decimal(self.product.price_taka * self.quantity)
        except TypeError as t:
            self.total += float(self.product.price_taka * self.quantity)

    @property
    def seller(self):
        return self.product.seller





