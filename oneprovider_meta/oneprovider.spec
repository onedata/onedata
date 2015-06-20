Name:		oneprovider
Version:    1.0.1.22.ged6c82a
Release:	1%{?dist}
Summary:    Meta package for oneprovider components
License:    MIT
URL:        https://onedata.org

Requires: op-ccm = 0.0.1.44.ga94a688
Requires: op-worker = 1.0.0.456.g4123c32
Requires: op-panel = 1.0.0.76.gefb4b86
Requires: riak

%description

%prep
%setup -c -T
%install
%files
