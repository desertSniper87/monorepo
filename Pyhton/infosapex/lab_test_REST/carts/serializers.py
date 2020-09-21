from rest_framework import serializers

from .models import Cart


class CartSerializer(serializers.HyperlinkedModelSerializer):
    total = serializers.ReadOnlyField()
    product_name = serializers.CharField(
        read_only=True, source='product.name'
    )
    price_per_unit = serializers.CharField(
        read_only=True, source='product.price_taka'
    )
    class Meta:
        model = Cart
        fields = ['product', 'quantity', 'price_per_unit',
                  'total', 'product_name']

    def __init__(self, *args, **kwargs):
        super(CartSerializer, self).__init__(*args, **kwargs)



