from rest_framework import serializers

from products.models import ProductCategory
from .models import Product


class ProductSerializer(serializers.HyperlinkedModelSerializer):
    category = serializers.CharField()
    seller = serializers.CharField(read_only=True)
    id = serializers.ReadOnlyField()

    class Meta:
        model = Product
        fields = ('id', 'image', 'name', 'description', 'seller', 'available_quantity', 'price_taka', 'category', 'timestamp')

    def create(self, validated_data):
        user = self.context['request'].user
        validated_data['seller'] = user

        category = ProductCategory.objects.get(name=validated_data['category'])
        validated_data['category'] = category

        return super(ProductSerializer, self).create(validated_data)

    def update(self, instance, validated_data):
        return super(ProductSerializer, self).update(instance, validated_data)


class ProductCategorySerializers(serializers.HyperlinkedModelSerializer):
    id = serializers.ReadOnlyField()
    class Meta:
        model = ProductCategory
        fields = '__all__'

