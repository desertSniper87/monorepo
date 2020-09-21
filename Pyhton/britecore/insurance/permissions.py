from rest_framework.permissions import BasePermission


class UserIsOwnerRisk(BasePermission):

    def has_object_permission(self, request, view, risk):
        return request.user.id == risk.user.id
