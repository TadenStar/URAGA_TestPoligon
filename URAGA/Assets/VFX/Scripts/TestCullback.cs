using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TestCullback : MonoBehaviour

{
    [SerializeField]
    private GameObject rightPuddle;

    [SerializeField]
    private GameObject leftPuddle;

    public void PuddleRight()
    {
        foreach (var particle in rightPuddle?.transform.GetComponentsInChildren<ParticleSystem>())
        {

            particle.Play();

        }

        Debug.Log("RightPuddle");
    }

    public void PuddleLeft()
    {
        foreach (var particle in leftPuddle?.transform.GetComponentsInChildren<ParticleSystem>())
        {

            particle.Play();

        }

        Debug.Log("LeftPuddle");
    }
}
