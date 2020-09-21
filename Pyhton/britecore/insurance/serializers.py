from django.contrib.auth.models import User, Group
from rest_framework import serializers

from insurance.models import Risk, RiskType


class UserSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = User
        fields = ('url', 'username', 'email', 'groups')


class GroupSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Group
        fields = ('url', 'name')


class RiskSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = Risk
        fields = ('name', 'date_created', 'risk_type', 'risk_attributes')

    def create(self, validated_data):
        user = self.context['request'].user
        validated_data['user'] = user
        return super(RiskSerializer, self).create(validated_data)

    def update(self, instance, validated_data):
        return super(RiskSerializer, self).update(instance, validated_data)


class RiskTypeSerializer(serializers.HyperlinkedModelSerializer):
    class Meta:
        model = RiskType
        fields = ('name', 'created_by', 'attribute_collection')

    def create(self, validated_data):
        created_by = self.context['request'].user
        validated_data['created_by'] = created_by
        return super(RiskTypeSerializer, self).create(validated_data)

    def update(self, instance, validated_data):
        return super(RiskTypeSerializer, self).update(instance, validated_data)
