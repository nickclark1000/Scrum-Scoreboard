$('#myModal').on('hidden.bs.modal', function () {
    $(this).find('label,input,textarea').val('');
});