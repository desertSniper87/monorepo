#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 23.02.2018
# Last Modified Date: 23.02.2018
from django.db import models
from django.db.models.signals import pre_save, post_save
from django.shortcuts import render, redirect
import math

from carts.models import Cart
from maalamaal.utils import unique_order_id_generator

ORDER_STATUS_CHOICES = (
                        ('created', 'Created'),
                        ('paid', 'Paid'),
                        ('shipped', 'Shipped'),
                        ('refunded', 'Refunded'), 
                       )

class Order(models.Model):
    order_id         = models.CharField(max_length = 120, blank=True)
    cart             = models.ForeignKey(Cart, on_delete='Cascade')
    status           = models.CharField(max_length=120, default='created')
    order_total      = models.DecimalField(default=0.00, max_digits=100, decimal_places=2)
    shipping_total   = models.DecimalField(default=1.00, max_digits=100, decimal_places=2)

    def __str__(self):
        return self.order_id

    def update_total(self):
        # cart_total          = self.cart.total
        cart_total          = self.cart.tax_total
        shipping_total      = self.shipping_total
        new_total           = math.fsum([cart_total,  shipping_total])
        new_total_formatted = format(new_total, '.2f')
        self.order_total    = new_total_formatted
        self.save()
        return new_total



def pre_save_create_order_id(sender, instance, *args, **kwargs):
    if not instance.order_id:
        instance.order_id = unique_order_id_generator(instance)

pre_save.connect(pre_save_create_order_id, sender=Order)

def post_save_cart_total(sender, instance, created, *args, **kwargs):
    if not created:                     #The sender is Cart. Cart pre-exist. So we find the cart.
        cart_obj   = instance
        cart_total = cart_obj.total
        cart_id    = cart_obj.id

        qs = Order.objects.filter(cart__id=cart_id)

        if qs.count() == 1:
            order_obj = qs.first()
            order_obj.update_total()

post_save.connect(post_save_cart_total, sender=Cart)

def post_save_new_cart_total(sender, instance, created, *args, **kwargs):
    if created:                          # No cart existed
        instance.update_total()

post_save.connect(post_save_new_cart_total, sender=Order)


