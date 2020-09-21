# from django.http import HttpResponse
from django.shortcuts import render, redirect
# from django.contrib.auth import authenticate, login, get_user_model
# from .forms import ContactForm, LoginForm, RegisterForm
from rest_framework import viewsets
from .serializers import UserSerializer, GroupSerializer
from django.contrib.auth.models import User, Group

def home_page(request):
    # if request.user.is_authenticated is True:

    context = {}
    return render(request, "home_page.html", context)


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
