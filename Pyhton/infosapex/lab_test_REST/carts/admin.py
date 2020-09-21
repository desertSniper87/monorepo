from django.contrib import admin
from .models import Cart

class CartAdmin(admin.ModelAdmin):
    list_display = ['user', 'quantity', 'total']


admin.site.register(Cart, CartAdmin)