from django import template
from fertilizer_shops.models import Fertilizer_shop

register = template.Library()

def narrow_down_shop(upazila, district, division):
    print(fertilizer_shops.objects.get(Q(division__icontains="Dhaka")))
    return True
