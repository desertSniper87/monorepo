from django.contrib import admin
from .models import Product, ProductCategory

class CategoryAdmin(admin.ModelAdmin):
    list_display = ['name']

class ProductAdmin(admin.ModelAdmin):
    list_display = ['name', 'category', 'available_quantity', 'price_taka']


admin.site.register(ProductCategory, CategoryAdmin)
admin.site.register(Product, ProductAdmin)