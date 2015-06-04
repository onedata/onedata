Name:		oneprovider
Version:    1.0.0
Release:	1%{?dist}
Summary:    Meta package for oneprovider components
License:    MIT
URL:        https://onedata.org

Requires:   op-ccm, op-worker, op-onepanel, riak

%description

%prep
%setup -c -T
%install
%files
