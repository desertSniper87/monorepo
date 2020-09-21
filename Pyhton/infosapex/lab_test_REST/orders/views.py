from django.db import transaction
from django.dispatch import Signal
from rest_framework import viewsets, status
from rest_framework.response import Response

from .models import Order
from .serializers import OrderSerializer
from carts.models import Cart


class OrderViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows groups to be viewed or edited.
    """
    queryset = Order.objects.all()
    serializer_class = OrderSerializer

    @transaction.atomic
    def create(self, request, *args, **kwargs):
        user = request.user
        user_carts = Cart.objects.filter(user=user, ordered=False)
        order_total = 0

        for cart in user_carts:
            cart.ordered = True
            order_total += cart.total
            cart.save(delete_old_cart=False)

            cart.product.available_quantity -= cart.quantity
            cart.product.save()

        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        # self.perform_create(serializer)
        serializer.save(
            user=user,
            order_total=order_total,
            carts=user_carts,
            paid=True
        )
        headers = self.get_success_headers(serializer.data)
        return Response(serializer.data, status=status.HTTP_201_CREATED, headers=headers)

    def list(self, request, *args, **kwargs):
        user = request.user
        user_type = request.user.groups.all().first().name
        if user_type == 'seller':
            print('Order may contain carts of other sellers')
            queryset = Order.objects.filter(carts__product__seller_id=user.id)
        else:
            queryset = Order.objects.filter(user=user)
        page = self.paginate_queryset(queryset)
        if page is not None:
            serializer = self.get_serializer(page, many=True)
            return self.get_paginated_response(serializer.data)

        serializer = self.get_serializer(queryset, many=True)
        return Response(serializer.data)








