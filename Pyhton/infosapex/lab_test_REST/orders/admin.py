from django.contrib import admin
from .models import Order

class OrderAdmin(admin.ModelAdmin):
    # list_display = ['user', 'product', 'quantity', 'total']
    pass

admin.site.register(Order, OrderAdmin)