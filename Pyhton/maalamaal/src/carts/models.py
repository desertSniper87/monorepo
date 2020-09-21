#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 12.02.2018
# Last Modified Date: 24.02.2018
from django.db import models

from products.models import Product
from django.conf import settings
from django.db.models.signals import pre_save, post_save, m2m_changed
from decimal import Decimal

User = settings.AUTH_USER_MODEL


class CartManager(models.Manager):
    def new_or_get(self, request):
        cart_id = request.session.get("cart_id", None)
        qs = self.get_queryset().filter(id=cart_id)

        if qs.count()==1:
            print("Cart ID exists")
            print(cart_id)
            new_obj = False
            cart_obj = qs.first()
            if request.user.is_authenticated == True and cart_obj.user is None:
                cart_obj.user = request.user
                cart_obj.save()

        else:
            new_obj = True
            print('create new cart')
            cart_obj = Cart.objects.new_cart(user=request.user)
            request.session['cart_id']=cart_obj.id
            print("Cart_ID: ", request.session.get("cart_id"))

        return cart_obj, new_obj

        
    
    def new_cart(self, user=None):
        print("User", user)
        user_obj = None
        if user is not None:
            if user.is_authenticated is True:
                user_obj = user
        return self.model.objects.create(user=user_obj)

class Cart(models.Model):
    """Docstring for Cart. """
    user        = models.ForeignKey(User, null=True, blank=True, on_delete=models.CASCADE)
    products    = models.ManyToManyField(Product, blank=True)
    total       = models.DecimalField(default=0.00, max_digits=100, decimal_places=2)
    tax_total   = models.DecimalField(default=0.00, max_digits=100, decimal_places=2)
    updated     = models.DateTimeField(auto_now=True)
    timestamp   = models.DateTimeField(auto_now_add=True)

    objects = CartManager()

    def __str__(self):
        return str(self.id)
    
def m2m_save_cart_receiver(sender, instance, action, *args, **kwargs):
    # if not instance.slug:
        # instance.slug = unique_slug_generator(instance)
    print("action: ", action)
    print("instance.products: ", instance.products.all())
    print("instance.total: ", instance.total)
    if action=='post_add' or action=='post_remove' or action=='post_clear':
        products = instance.products.all() 
        total = 0

        for x in products:
            print("x: ", x)
            total += x.price

        print('total: ', total)
        instance.total = total
        instance.save()

m2m_changed.connect(m2m_save_cart_receiver, sender=Cart.products.through)

def pre_save_cart_receiver(sender, instance, *args, **kwargs):
        print(type(instance.total))
        new_tax_total      = round(float(instance.total)* 1.15)
        instance.tax_total = new_tax_total

pre_save.connect(pre_save_cart_receiver, sender=Cart)
