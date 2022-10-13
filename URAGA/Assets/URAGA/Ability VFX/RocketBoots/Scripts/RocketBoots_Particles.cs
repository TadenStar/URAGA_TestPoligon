using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RocketBoots_Particles : MonoBehaviour

{
    [SerializeField]
    private GameObject particles_01;

    [SerializeField]
    private GameObject particles_02;

    public void StartFlame()
    {
        foreach (var particle in particles_01?.transform.GetComponentsInChildren<ParticleSystem>())
        {

            particle.Play();

        }
    }
    public void EndFlame()
    {
        foreach (var particle in particles_02?.transform.GetComponentsInChildren<ParticleSystem>())
        {

            particle.Play();

        }
    }
}
