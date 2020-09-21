from django.contrib.auth.models import User

from django.db import models


class ProductCategory(models.Model):
    name = models.CharField(max_length=120)

    def __str__(self):
        return self.name

class Product(models.Model):
    image = models.ImageField()
    name = models.CharField(max_length=120)
    description = models.TextField()
    # slug = models.SlugField(blank=True, unique=True)
    seller = models.ForeignKey(User, on_delete=models.CASCADE)
    available_quantity = models.PositiveIntegerField(default=0)
    price_taka = models.DecimalField(decimal_places = 2, max_digits = 10, default=39.99)
    category = models.ForeignKey(ProductCategory,
                                 null=True, blank=True,
                                 on_delete=models.SET_NULL)
    timestamp = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return self.name