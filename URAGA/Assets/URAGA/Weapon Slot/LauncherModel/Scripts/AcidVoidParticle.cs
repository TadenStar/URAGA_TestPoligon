using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AcidVoidParticle : MonoBehaviour
{
    public ParticleSystem particleSystem;

    public GameObject acidExplosion;

    List<ParticleCollisionEvent> colEvent = new List<ParticleCollisionEvent>();

    private void Start()
    {
        particleSystem = GetComponent<ParticleSystem>();
    }

    private void Update()
    {
        if(Input.GetKeyDown(KeyCode.Mouse0))
        {
            particleSystem.Play();
        }    
    }
    private void OnParticleCollision(GameObject other)
    {
        int events = particleSystem.GetCollisionEvents(other, colEvent);

        for (int i = 0; i < events; i++)
        {
            Instantiate(acidExplosion, colEvent[i].intersection, Quaternion.LookRotation(colEvent[i].normal));
        }
    }
}
