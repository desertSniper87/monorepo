from rest_framework import serializers
from .models import Fertilizer_shop


class FertilizerShopSerlializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Fertilizer_shop
        fields = ('id', 'name', 'village', 'union_council',\
                'upazila', 'district', 'division')



