from rest_framework import viewsets
from rest_framework.permissions import IsAuthenticated

from .models import Product, ProductCategory
from .serializers import ProductSerializer, ProductCategorySerializers


class ProductViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows products to be viewed or edited.
    """
    queryset = Product.objects.all()
    serializer_class = ProductSerializer

    permission_classes_by_action = {'create': [IsAuthenticated]}

    def create(self, request, *args, **kwargs):
        return super(ProductViewSet, self).create(request, *args, **kwargs)

    def get_permissions(self):
        try:
            # return permission_classes depending on `action`
            return [permission() for permission in self.permission_classes_by_action[self.action]]
        except KeyError:
            # action is not set return default permission_classes
            return [permission() for permission in self.permission_classes]

    def list(self, request, *args, **kwargs):
        category = request.GET.get('category')
        if category:
            self.queryset = Product.objects.filter(category=category)
        user = request.user
        user_type = user.groups.first()
        if user_type and user_type.name == 'seller':
            self.queryset = self.queryset.filter(seller=user)

        return super(ProductViewSet, self).list(request, *args, **kwargs)


class ProductCategoryViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows product categories to be viewed or edited.
    """
    http_method_names = ['get', 'head']
    queryset = ProductCategory.objects.all()
    serializer_class = ProductCategorySerializers
