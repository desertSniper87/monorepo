#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author            : desertsniper87 <torshobuet@gmail.com>
# Date              : 20.02.2018
# Last Modified Date: 30.03.2018
from django.shortcuts import render, redirect
from django.http import JsonResponse

from products.models import Product
from .models import Cart
from orders.models import Order


def cart_detail_api_view(request):
    cart_obj, new_obj= Cart.objects.new_or_get(request)
    products = [{
                 "id"    : x.id, 
                 "name"  : x.name,
                 "price" : x.price,
                 "url"   : x.get_absolute_url()
                }\
            for x in cart_obj.products.all()]
    print("products: ", products)
    cart_data = {
                 "products"  : products,
                 "total"     : cart_obj.total,
                 "tax_total" : cart_obj.tax_total
                }

    return JsonResponse(cart_data)


def cart_create(user=None):
    cart_obj = Cart.objects.get(user=None)
    return cart_obj

def cart_home(request): # why there is no self arg?
    cart_obj, new_obj= Cart.objects.new_or_get(request)

    products = cart_obj.products.all() 
    total = 0

    for x in products:
        total += x.price

    print('total: ', total)
    cart_obj.total = total
    cart_obj.save()

    return render(request, "carts/home.html", {"cart": cart_obj})

def cart_update(request):
    print("request.POST: ", request.POST)
    product_id = request.POST.get('product_id')
    if product_id is not None:
        try:
            product_obj = Product.objects.get(id=product_id)
        except Product.DoesNotExist:
            return redirect("carts:home")
    cart_obj, new_obj = Cart.objects.new_or_get(request)
    # cart_obj.products.add(product_obj)
    # cart_obj.title
    print("cart_obj, new_obj: ", cart_obj, new_obj)
    if product_obj in cart_obj.products.all():
        cart_obj.products.remove(product_obj)
        added = False
    else:
        print("ADDING")
        cart_obj.products.add(product_obj)
        added = True
    cart_obj.save()

    if request.is_ajax():
        print("Request is ajax")
        json_data = {
                     "added": added,
                     "removed": not added,
                     "cartItemCount" : cart_obj.products.count(),
                    }
        # print("json_data: ", json_data)
        print("cart_obj.products.count: ", cart_obj.products.count())
        request.session['cart_items'] = cart_obj.products.count()
        return JsonResponse(json_data, status=200)
        # return JsonResponse({"message": "Error 400"}, status_code=400)

    request.session['cart_items'] = cart_obj.products.count()

    # return redirect(product_obj.get_absolute_url())
    return redirect("carts:home")

def checkout_home(request):
    cart_obj, cart_obj_created = Cart.objects.new_or_get(request)
    order_obj = None
    if not cart_obj_created and cart_obj.products.count()!=0:
        order_obj, new_order_obj_created = Order.objects.get_or_create(cart=cart_obj)
    else:
        return redirect("cart:home")


    return render(request, "carts/checkout.html", {"order": order_obj})
