from django import forms
from django.contrib.auth import get_user_model

User = get_user_model()


class ContactForm(forms.Form):
    full_name = forms.CharField(
        widget=forms.TextInput(
            attrs={"class": "form-control",
                   "id": "form_full_name",
                   "placeholder": "Your full name"}))
    email = forms.EmailField(
        widget=forms.EmailInput(
            attrs={"class": "form-control",
                   "id": "form_email",
                   "placeholder": "Your Email"}))
    content = forms.CharField(
        widget=forms.Textarea(
            attrs={"class": "form-control",
                   "id": "form_content",
                   "placeholder": "Your content"}))

    def clean_email(self):
        email = self.cleaned_data.get("email")
        if not "gmail.com" in email:
            raise forms.ValidationError("Email has to be gmail.")
        return email

    def clean_content(self):
        content = self.cleaned_data.get("content")
        print("len(content): ", len(content))
        if len(content)<10:
            raise forms.ValidationError("Content has to be at least 10\
                    Characters long")
        return content

class LoginForm(forms.Form):
    username = forms.CharField()
    password = forms.CharField(widget=forms.PasswordInput())

class RegisterForm(forms.Form):
    username = forms.CharField()
    email = forms.EmailField()
    password = forms.CharField(widget=forms.PasswordInput())
    password2 = forms.CharField(label = 'Confirm Password', widget=forms.PasswordInput())

    def clean_username(self):
        username = self.cleaned_data.get("username")
        qs = User.objects.filter(username=username)
        if qs.exists():
            raise forms.ValidationError("Username is taken")
        return username

    def clean_email(self):
        email = self.cleaned_data.get("email")
        qs = User.objects.filter(email=email)
        if qs.exists():
            raise forms.ValidationError("Email is taken")
        return email

    def clean(self):
        data = self.cleaned_data
        password = self.cleaned_data.get('password')
        password2 = self.cleaned_data.get('password2')
        if password2 != password:
            raise forms.ValidationError("Passwords must match")

        print(data)
        return data

