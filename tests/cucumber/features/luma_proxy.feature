Feature: LUMA proxy test

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Operations on POSIX storage
    When u1 creates regular files [posix/file1]
    And u1 writes "TEST TEXT ONEDATA POSIX" to posix/file1
    Then u1 reads "TEST TEXT ONEDATA POSIX" from posix/file1

  Scenario: Operations on CEPH storage
    When u1 creates regular files [ceph/file1]
    And u1 writes "TEST TEXT ONEDATA CEPH" to ceph/file1
    Then u1 reads "TEST TEXT ONEDATA CEPH" from ceph/file1

  Scenario: Operations on Amazon S3 storage
    When u1 creates regular files [s3/file1]
    And u1 writes "TEST TEXT ONEDATA S3" to s3/file1
    Then u1 reads "TEST TEXT ONEDATA S3" from s3/file1

  Scenario: Operations on Openstack Swift storage  
    When u1 creates regular files [swift/file1]
    And u1 writes "TEST TEXT ONEDATA S3" to swift/file1
    Then u1 reads "TEST TEXT ONEDATA S3" from swift/file1    