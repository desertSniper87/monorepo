from rest_framework import serializers

from carts.serializers import CartSerializer
from .models import Order


class OrderSerializer(serializers.HyperlinkedModelSerializer):
    id = serializers.ReadOnlyField()
    order_total = serializers.ReadOnlyField()
    user = serializers.ReadOnlyField(source='user.username')
    carts = CartSerializer(many=True, read_only=True)
    paid = serializers.ReadOnlyField()
    timestamp = serializers.DateTimeField(read_only=True, format="%d %b, %H:%M")

    class Meta:
        model = Order
        depth = 1
        fields = ['id', 'order_total', 'user', 'paid', 'carts', 'timestamp']


