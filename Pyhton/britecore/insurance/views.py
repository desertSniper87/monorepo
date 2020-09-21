from django.contrib.auth.models import User, Group
from rest_framework import viewsets

from insurance.models import Risk, RiskType
from insurance.serializers import UserSerializer, GroupSerializer, RiskSerializer, RiskTypeSerializer


class UserViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows users to be viewed or edited.
    """
    queryset = User.objects.all().order_by('-date_joined')
    serializer_class = UserSerializer


class GroupViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows groups to be viewed or edited.
    """
    queryset = Group.objects.all()
    serializer_class = GroupSerializer


class RiskViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows risks to be viewed or edited.
    """
    queryset = Risk.objects.all()
    serializer_class = RiskSerializer

class RiskTypeViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows risks to be viewed or edited.
    """
    queryset = RiskType.objects.all()
    serializer_class = RiskTypeSerializer


