from django import forms

class SignupForm(forms.Form):
    # first_name = forms.CharField(max_length=50, label='Voornaam')
    # last_name = forms.CharField(max_length=50, label='Achternaam')

    name = forms.CharField(label='Name of User', required=True, max_length=50)
    gender = forms.CharField(label='Gender', required=True, max_length=1)
    present_address = forms.CharField(label='Present Address', required=False, max_length=80)
    permanent_address = forms.CharField(label='Permanent Address', required=False, max_length=80)
    father_name = forms.CharField(label='Father\'s name of User', required=False, max_length=50)
    mother_name = forms.CharField(label='Mother\'s name of User', required=False, max_length=50)
    phone_number =forms.IntegerField(required=False)
    # email = forms.EmailField(required=True)
    picture = forms.FileField(required=False)

    def signup(self, request, user):
        # user.first_name = self.cleaned_data['first_name']
        # user.last_name = self.cleaned_data['last_name']
        # user.save()

        user.name = forms.CharField(label='Name of User', required=True, max_length=50)
        user.gender = forms.CharField(label='Gender', required=True, max_length=1)
        user.present_address = forms.CharField(label='Present Address', required=False, max_length=80)
        user.permanent_address = forms.CharField(label='Permanent Address', required=False, max_length=80)
        user.father_name = forms.CharField(label='Father\'s name of User', required=False, max_length=50)
        user.mother_name = forms.CharField(label='Mother\'s name of User', required=False, max_length=50)
        user.phone_number =forms.IntegerField(required=False)
        # user.email = forms.EmailField(required=True)
        user.picture = forms.FileField(label='Picture', required=False)
        user.save()
